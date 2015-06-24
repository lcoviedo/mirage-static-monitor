open Lwt
open Printf
open V1_LWT
open OS

let red fmt    = sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = sprintf ("\027[36m"^^fmt^^"\027[m")

(* Initialise values for timestamps and array*)
let t0 = ref 0.0
let t1 = ref 0.0
let i = ref 0
let d_array = Array.make 64 0.0

let high_threshodld = 0.00007
let irmin_ip = "10.0.0.1"

let uri = Uri.of_string "http://irmin:8080/update/jitsu/vm/"    (* Path for vm requests*)
let add_vm = `String "{\"params\":\"add_vm\"}"                  (* Parameter to be defined *)

module Main (C:CONSOLE) (FS:KV_RO) (S:STACKV4) = struct

  module H   = Cohttp_mirage.Server(Conduit_mirage.Flow)
  module HTTP = Cohttp_mirage.Client
   
  let conduit = Conduit_mirage.empty
  let stackv4 = Conduit_mirage.stackv4 (module S)
  
  (* Location of Irmin store *) 
  let irmin_store =
    let hosts = Hashtbl.create 3 in
    Hashtbl.add hosts "irmin"
      (fun ~port -> `TCP (Ipaddr.of_string_exn irmin_ip, port));
    hosts  
    
  (* Store a parameter on Irmin *)     
  let http_post c ctx =
    C.log_s c (sprintf "Posting a value %s in Irmin" (Uri.to_string uri)) >>= fun () ->
    HTTP.post ~ctx ~body:add_vm uri >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string add_vm >>= fun body ->
    C.log_s c (sprintf "%s" body)
   
  (* Monitor load based on request/reply times *)
  (** TODO  scaling down*)
  let monitoring c stack =  
    Lwt.return (
      t1 := Time.Monotonic.to_seconds(Time.Monotonic.time());       
      let delay = !t1 -. !t0 in
      d_array.(!i) <- delay;
      (* C.log c (Printf.sprintf "t0 = %f    t1= %f    delay = %f" !t0 !t1 delay); *)   (* For debugging *)
      if !i < 63 then i := !i + 1 else begin
        i := 0;      
        let avg = (Array.fold_right (+.) d_array 0.0) /. float(Array.length d_array) in
        if avg > high_threshold then (                                          (* above that means ~ %80 cpu utilisation and likely to crash*)
          C.log c (Printf.sprintf "HIGH LOAD.......%f" avg);                            (* For debugging *)
          Lwt.ignore_result (
            lwt conduit = Conduit_mirage.with_tcp Conduit_mirage.empty stackv4 stack in
            let res = Resolver_mirage.static irmin_store in
            let ctx = HTTP.ctx res conduit in
            http_post c ctx) )
        end)

  let start c fs stack = 
    let read_fs name =
      FS.size fs name >>= function
      | `Error (FS.Unknown_key _) -> fail (Failure ("read " ^ name))
      | `Ok size ->
        FS.read fs name 0 (Int64.to_int size) >>= function
        | `Error (FS.Unknown_key _) -> fail (Failure ("read " ^ name))
        | `Ok bufs -> return (Cstruct.copyv bufs)
    in

    (* Split a URI into a list of path segments *)
    let split_path uri =
      let path = Uri.path uri in    
      let rec aux = function
        | [] | [""] -> []
        | hd::tl -> hd :: aux tl
      in
      List.filter (fun e -> e <> "")
        (aux (Re_str.(split_delim (regexp_string "/") path)))
    in
    
    (* dispatch non-file URLs *)
    let rec dispatcher = function
      | [] | [""] -> dispatcher ["index.html"]
      | segments -> 
        let path = String.concat "/" segments in 
        Lwt.catch (fun () -> 
          read_fs path
          >>= fun body ->                                                                       
          H.respond_string ~status:`OK ~body () 
          )  (fun exn -> 
          H.respond_not_found ()
          )
          in 
    (* HTTP callback *)
    let callback conn_id request body =    
      t0 := Time.Monotonic.to_seconds(Time.Monotonic.time());
      let uri = Cohttp.Request.uri request in
      let reply = dispatcher (split_path uri) in
      let _ = monitoring c stack in
      reply in
      Conduit_mirage.with_tcp conduit stackv4 stack >>= fun conduit ->
      let spec = H.make ~callback:callback () in
      Conduit_mirage.listen conduit (`TCP 80) (H.listen spec)
          
end
