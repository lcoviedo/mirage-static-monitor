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
let t2 = ref 0.0
let i = ref 0
let d_array = Array.make 64 0.0
let counter = ref 0
let cid1 = ref 0

let h_load = 0.00007                                                                (* High threshold value *)
let irmin_ip = "10.0.0.1"                                                           (* Location of irmin store *)

let uri = Uri.of_string "http://irmin:8080/update/jitsu/vm/request/"                (* Path for vm requests*)
let add_vm = `String "{\"params\":\"add_vm\"}"                                      (* Parameters to be defined *)
let destroy_vm = `String "{\"params\":\"destroy_vm\"}"


module Main (C:CONSOLE) (FS:KV_RO) (S:STACKV4) = struct

  module H   = Cohttp_mirage.Server(Conduit_mirage.Flow)
  module HTTP = Cohttp_mirage.Client
   
  let conduit = Conduit_mirage.empty
  let stackv4 = Conduit_mirage.stackv4 (module S)
   
  (* Manual resolve for irmin*) 
  let irmin_store =
    let hosts = Hashtbl.create 3 in
    Hashtbl.add hosts "irmin"
      (fun ~port -> `TCP (Ipaddr.of_string_exn irmin_ip, port));
    hosts  
    
  (* Store a parameter on Irmin *)     
  let http_post c ctx req=
    C.log_s c (sprintf "Posting a value %s in Irmin" (Uri.to_string uri)) >>= fun () ->
    HTTP.post ~ctx ~body:req (*add_vm*) uri >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string req (*add_vm*) >>= fun body ->
    C.log_s c (sprintf "%s" body)
   
  (* conduit *)
  let conduit_conn c stack req=
    Lwt.ignore_result (
      lwt conduit = Conduit_mirage.with_tcp Conduit_mirage.empty stackv4 stack in
        let res = Resolver_mirage.static irmin_store in
        let ctx = HTTP.ctx res conduit in
        http_post c ctx req) 
   
  (* Monitor load based on request/reply times *)
  let scale_up c stack =  
    Lwt.return (
      t1 := Time.Monotonic.to_seconds(Time.Monotonic.time());       
      let delay = !t1 -. !t0 in
      d_array.(!i) <- delay;
      (*C.log c (Printf.sprintf "t0 = %f  t1= %f  delay = %f" !t0 !t1 delay);*)       (* For debugging *)                
      if !i < (Array.length d_array - 1) 
        then incr i 
        else (
        i := 0;      
        let avg = 
          (Array.fold_right (+.) d_array 0.0) /. float(Array.length d_array) in
        if avg > h_load then (                                                        (* above that ~ %80 cpu and likely to crash*)
          (*C.log c (Printf.sprintf "HIGH LOAD.......%f" avg);*)                      (* For debugging *)
          conduit_conn c stack add_vm
        )) )
        
  (* Monitor idleness of unikernel *)        
  let rec scale_down c stack n =                                                     (* if idle for 5 secs then trigger action *)
      cid1 := !counter;                                                              (* !counter holds the current value of cids *)
      Time.sleep n >>= fun () ->   
        if !cid1 = !counter then (
          C.log c (Printf.sprintf "LOW LOAD -> destroy replica");                    (* For debugging *)           
          conduit_conn c stack destroy_vm
          )
          else C.log c (
                 Printf.sprintf "New conns in last 5s: %d" (!counter - !cid1));      (* For debugging *)
      scale_down c stack n 

  let start c fs stack = 
   Lwt.join[(
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
          read_fs path >>= fun body ->                                                                       
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
      let _ = scale_up c stack in
      reply 
    in 
    
    let conn_closed (_,conn_id) =
      let cid = Cohttp.Connection.to_string conn_id  in
      C.log c (Printf.sprintf "conn %s closed" cid);
      counter := int_of_string(cid); 
      in
        
      Conduit_mirage.with_tcp conduit stackv4 stack >>= fun conduit ->
      let spec = H.make ~conn_closed ~callback:callback () in
      Conduit_mirage.listen conduit (`TCP 80) (H.listen spec)); 
      (scale_down c stack 5.0)]                                             (* Scale down monitor thread *)
          
end
