open Lwt
open Printf
open V1_LWT
open OS
open Fractal_client

let red fmt    = sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = sprintf ("\027[36m"^^fmt^^"\027[m")

let exp_no = ref 0
let rt = ref 0.0
let t_req = ref 0.0
let k = ref 0
let rps_array = Array.make 10 (0.0, 0)
let stats_array = Array.make 10 (0.0, 0, "")
let rt_array = Array.make 64 0.0
let obj_count = ref 0
let oid = ref 0
let uri_req = (Uri.of_string ("http://irmin/update/jitsu/request/"))
let uri_results = "http://irmin:8080/update/jitsu/exp/"
let replicate_flag = ref false
let high_load = 1000  (* 1000 requests per second - High threshold value *)
let low_load = 100  (* 100 requests per second rate - Low threshold value *)
let irmin_ip = "10.0.0.1"  (* Location of irmin store *)
let irmin_port = ref 0
let irmin_1 = ["12:43:3d:a3:d3:02"; "12:43:3d:a3:d3:03"; "12:43:3d:a3:d3:04"; "12:43:3d:a3:d3:05"; 
               "12:43:3d:a3:d3:06"; "12:43:3d:a3:d3:07"; "12:43:3d:a3:d3:08"; "12:43:3d:a3:d3:09";
               "12:43:3d:a3:d3:28"; "c0:ff:ee:c0:ff:ee";
               ]
let irmin_2 = ["12:43:3d:a3:d3:14"; "12:43:3d:a3:d3:15"; "12:43:3d:a3:d3:16"; "12:43:3d:a3:d3:17"; 
               "12:43:3d:a3:d3:18"; "12:43:3d:a3:d3:19"; "12:43:3d:a3:d3:1a"; "12:43:3d:a3:d3:1b";
               ]

module Main (C:CONSOLE) (FS:KV_RO) (S:STACKV4) (N0:NETWORK) = struct

  module H   = Cohttp_mirage.Server(Conduit_mirage.Flow)
  module HTTP = Cohttp_mirage.Client
   
  let conduit = Conduit_mirage.empty
  let stackv4 = Conduit_mirage.stackv4 (module S)

(* Host table to resolve local and initial irmin *)  
  let host_table =
    Hashtbl.create 3

  let irmin_task uid =
     "{\"task\":{\"date\":\"0\",\"uid\":\"" ^ 
       (string_of_int uid) ^ 
         "\",\"owner\":\"\",\"messages\":[\"write\"]},\"params\":\""
  
(** Move http_get *)
  (* Conduit connection helper *) 
  let conduit_conn stack =
    lwt conduit = Conduit_mirage.with_tcp conduit stackv4 stack in
      let res = Resolver_mirage.static host_table in
      let ctx = HTTP.ctx res conduit in 
      Lwt.return (ctx)

  let http_get c stack uri =
    lwt ctx = conduit_conn stack in
    C.log_s c (sprintf "Fetching %s:" (Uri.to_string uri)) >>= fun () -> (* debugging *)
    HTTP.get ~ctx uri >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun body ->
      (*let _ = C.log_s c (sprintf "Response: %s" (body)) in*) (* debugging *)
      let str = Re_str.replace_first (Re_str.regexp ".*\\[\"") "" body in
      let str_value = Re_str.replace_first (Re_str.regexp "\"\\].*") "" str in
      C.log_s c (sprintf "Value: %s" str_value) >>= fun () -> (* debugging *)
      Lwt.return(str_value)
  
  let rec monitoring spec t s uri n c =  (* n equals 10 which is the lenght of rps array*)
  (* get requests per second and response times *)
  if !k < (int_of_float n) then (
    t_req := Clock.time();
    Time.sleep 1.0 >>= fun () ->
    (* get_stats function from modified cohttp *)
    let stats = (H.get_stats spec) in
      let (failed, rps, active, rt_list) = stats in
      let rt_rpc = Rpc.Enum [
          Rpc.rpc_of_string (
            List.fold_right (fun (x, y) acc -> 
              (string_of_float x)^" "^(string_of_float y)^" ;"^acc) rt_list "")
        ] in
      let rt = Rpc.to_string rt_rpc in
    stats_array.(!k) <- (!t_req,rps,rt);
    incr k;
    (* Scale out *)
    if (rps >= high_load && !replicate_flag = true) then (
      replicate t >>= fun () ->
      replicate_flag := false;
      C.log c (sprintf "CREATE REPLICA........");
      monitoring spec t s uri n c
    )
    (* Scale back *)
    else if (rps <= low_load && !replicate_flag = true) then (
      die t >>= fun () ->
      replicate_flag := false;     
      C.log c (sprintf "DELETE REPLICA: %d" rps);
      monitoring spec t s uri n c
    )
    (* Halt event *) (**TODO -> Enable halt *)
    (*
    else if (rps < high_load && rps > low_load && rt >= 0.0001) then (
      halt t >>= fun () ->
      replicate_flag := false;
      C.log c (sprintf "HALT EVENT -> stop sending new traffic");
      monitoring spec t s uri n c
    )
    *)
    else (
      monitoring spec t s uri n c
    )
  )
  else ( (* Post stats results *)
    incr exp_no;
    C.log c (sprintf "Results case: %d" !exp_no); (* debugging *)
    k := 0; (** TODO -> Fix Uri for results*)
    let task = irmin_task !exp_no in
    let uri_results = Uri.of_string (uri ^ (string_of_int !exp_no)) in
    lwt t_results = 
      create_irmin_client (module S:V1_LWT.STACKV4 with type t = S.t) s uri_results host_table
      in   
    post_results t_results task stats_array >>= fun () ->
    monitoring spec t s uri n c
  )
 
  let rec replicate_timer n c =
    Time.sleep n >>= fun () -> 
    replicate_flag := true;
    replicate_timer n c

  (* START *)
  let start c fs stack n0 () =
    let vm_name = Macaddr.to_string (N0.mac n0) in
    let find_port = List.exists (fun x -> vm_name = x) irmin_2 in
    if find_port = true then (irmin_port := 8081 ) else (irmin_port := 8080);
    Hashtbl.add host_table "irmin"
      (fun ~port -> `TCP (Ipaddr.of_string_exn irmin_ip, !irmin_port));
    let uri = 
      (Uri.of_string ("http://irmin/update/read/jitsu/" ^ vm_name ^ "/initial_xs")) in (** double check uri *)
    lwt initial_xs = http_get c stack uri in
    Hashtbl.add host_table "initial_xs"     
      (fun ~port -> `TCP (Ipaddr.of_string_exn initial_xs, 8080));
    (* initialise t and t_results*)
    lwt t = 
      create_irmin_client (module S:V1_LWT.STACKV4 with type t = S.t) stack uri_req host_table in
    let uri_dataset = (uri_results ^ vm_name ^ "/data") in
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
        | ["hello"] -> dispatcher ["hello.html"]
        | ["1"] -> dispatcher ["p-1.html"]
        | ["2"] -> dispatcher ["p-3.html"]
        | ["3"] -> dispatcher ["p-4.html"]
        | ["4"] -> dispatcher ["p-7.html"]
        | ["5"] -> dispatcher ["p-15.html"]
        | ["6"] -> dispatcher ["p-27.html"]
        | ["7"] -> dispatcher ["p-38.html"]
        | segments -> 
          let path = String.concat "/" segments in 
          incr obj_count;
          Lwt.catch (fun () -> 
              read_fs path >>= fun body ->                
              H.respond_string ~status:`OK ~body ()   
            )  (fun exn ->
              H.respond_not_found ()
            )
      in
      (* HTTP callback *)
      let callback conn_id request body =
        let uri = Cohttp.Request.uri request in
        dispatcher (split_path uri)
      in
      (* result is added to comply with modified cohttp library *)
      let conn_closed (_,conn_id) (result) =
        let cid = Cohttp.Connection.to_string conn_id in
        (*C.log c (sprintf "conns closed: %s" cid);*)        
        ()
        in
      Conduit_mirage.with_tcp conduit stackv4 stack >>= fun conduit ->
      let spec = H.make ~conn_closed ~callback:callback () in
      Lwt.join [( 
          Conduit_mirage.listen conduit (`TCP 80) (H.listen spec));
          (replicate_timer 10.0 c); (** Change timer *)
          (monitoring spec t stack uri_dataset 10.0 c); (* Scale events thread *)
      ]
end
