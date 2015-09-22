open Lwt
open Printf
open V1_LWT
open OS

let red fmt    = sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = sprintf ("\027[36m"^^fmt^^"\027[m")

(* Initialise timestamps, array and cid counters*)
let t0 = ref 0.0
let t1 = ref 0.0
let t_avg = ref 0.0
let t_req = ref 0.0
(*let log = ref ""
let log_store = ref [|""|]*)
let i = ref 0
let j = ref 0
let k = ref 0
let req_sec_array = Array.make 5 (0.0, 0)
let delay_array = Array.make 64 0.0
let obj_count = ref 0
let oid = ref 0
let irmin_task = "{\"task\":{\"date\":\"0\",\"uid\":\"0\",\"owner\":\"\",\"messages\":[\"write\"]},\"params\":\""
let avg_flag = ref false  (*flag to sum up consecutive avg values once the 16 array is full*)
let sum_avg_array = Array.make 16 0.0
let avg_array = Array.make 128 (0.0, 0.0)  (* For expermiental purposes *)
let avg_counter = ref 0
let exp_no = ref 0
let replicate_flag = ref false
let high_load = 1000  (* 1000 requests per second - High threshold value *)
let low_load = 100  (* 100 requests per second rate - Low threshold value *)
(*let max_objreq = 385.0*)
let irmin_ip = "128.232.80.10" (*"10.0.0.1"*)  (* Location of irmin store *)
let irmin_port = ref 0
let irmin_1 = ["12:43:3d:a3:d3:02"; "12:43:3d:a3:d3:03"; "12:43:3d:a3:d3:04"; "12:43:3d:a3:d3:05"; 
               "12:43:3d:a3:d3:06"; "12:43:3d:a3:d3:07"; "12:43:3d:a3:d3:08"; "12:43:3d:a3:d3:09";
               "c0:ff:ee:c0:ff:ee"; "12:43:3d:a3:d3:28"; 
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

  (* Conduit connection helper *)
  let conduit_conn stack =
    lwt conduit = Conduit_mirage.with_tcp Conduit_mirage.empty stackv4 stack in
      let res = Resolver_mirage.static host_table in
      let ctx = HTTP.ctx res conduit in Lwt.return (conduit,ctx)
  
  let http_get c stack uri =
    lwt (conduit, ctx) = conduit_conn stack in
    (*C.log_s c (sprintf "Fetching %s:" (Uri.to_string uri)) >>= fun () ->*) (* debugging *)
    HTTP.get ~ctx uri >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun body ->
      (*let _ = C.log_s c (sprintf "Response: %s" (body)) in*) (* debugging *)
      let str = Re_str.replace_first (Re_str.regexp ".*\\[\"") "" body in
      let str_value = Re_str.replace_first (Re_str.regexp "\"\\].*") "" str in
      (*C.log_s c (sprintf "Value: %s" str_value) >>= fun () ->*) (* debugging *)
      Lwt.return(str_value)

  (* Send request/posts to Irmin *)
  let http_post c stack uri req =    
    Lwt.ignore_result (
      lwt (conduit, ctx) = conduit_conn stack in
      (*C.log_s c (sprintf "Posting in path %s" (Uri.to_string uri)) >>= fun () ->*) (* debugging *)
      HTTP.post ~ctx ~body:req uri >>= fun (resp, body) ->
      Cohttp_lwt_body.to_string req >>= fun body -> 
      C.log_s c (sprintf ("Posting in path %s") (Uri.to_string uri) ) (* debugging *)
      (*C.log_s c (sprintf ("Body:%s") body) (* debugging *) *)
      )
 
  (* Monitor-Scale down based on objects requested *)
  let rec monitoring c stack vm_name n =   
    if !k < (int_of_float n) then (
      oid := !obj_count;
      t_req := Clock.time();
      Time.sleep 1.0 >>= fun () ->
      let req_sec = (!obj_count - !oid) in
      req_sec_array.(!k) <- (!t_req,req_sec);
      incr k;
      let _ =
      (* Scale out *)
      if req_sec >= high_load then (
        let rpc_add = Rpc.Enum [
            Rpc.rpc_of_string "add_vm";
            Rpc.rpc_of_string "static-web";
            Rpc.rpc_of_string (sprintf "%f" !t_req);
          ] in
        let add = Rpc.to_string rpc_add in
        let add_vm = `String (irmin_task ^ add ^ "\"}") in
        let uri = (Uri.of_string ("http://irmin/update/jitsu/request/" ^ vm_name ^ "/request")) in
        http_post c stack uri add_vm;
        replicate_flag := false;
        C.log c (sprintf "CREATE REPLICA........")  (* For debugging *)
        )
      (* Scale back *)
      else if req_sec <= low_load then (
        let rpc_del = Rpc.Enum [
            Rpc.rpc_of_string "del_vm";
            (*Rpc.rpc_of_string vm_name;*)
            Rpc.rpc_of_string (sprintf "%f" !t_req);
          ] in
        let del = Rpc.to_string rpc_del in
        let del_vm = `String (irmin_task ^ del ^ "\"}") in
        let uri = (Uri.of_string ("http://irmin/update/jitsu/request/" ^ vm_name ^ "/request")) in
        (*http_post c stack uri del_vm;*)  (*TODO add control to delete requests *)
        C.log c (sprintf "LOW LOAD -> delete replica: %d" req_sec)  (* For debugging *)
      )
      else ()
      in
      monitoring c stack vm_name n
    )
    else (
      k := 0;
      incr exp_no;
      (*C.log c (sprintf "***  Data set #%d  ***" !exp_no);*)
      let rpc_req_sec = Rpc.Enum [
          Rpc.rpc_of_string (Array.fold_right (fun (x, y) acc ->
              (string_of_float x)^" "^(sprintf "%d" y)^";"^acc ) req_sec_array "")
        ] in
      let rpc_string = Rpc.to_string rpc_req_sec in
      let rpc_req = `String (irmin_task ^ rpc_string ^ "\"}") in
      let uri = (Uri.of_string ("http://irmin/update/jitsu/exp/" ^ vm_name ^ "/data" ^ (string_of_int !exp_no))) in
      C.log c (sprintf "DATA %d\n%s" !exp_no rpc_string);
      http_post c stack uri rpc_req;
      http_post c stack uri rpc_req;   
      monitoring c stack vm_name n
    )
   
    (* Read number of replicas
    (*
    let uri = (Uri.of_string ("http://initial_xs/read/jitsu/vm/static-web/num_of_reps")) in
    lwt num_of_reps = http_get c stack uri in
    *)
    (*C.log c (sprintf "Number of replicas: %s" num_of_reps);*) (* debugging *)
    let avg_objreq = (!obj_count - !oid)/5 in
    (*
    let low_val = (1.0 -. 1.0/float_of_string(num_of_reps)) *. (max_objreq *. 0.70) in
    if low_val <= 0.60 then ( 
    *)
    (*
    if avg_objreq < low_load then (
      let rpc_del = Rpc.Enum [
          Rpc.rpc_of_string "del_vm";
          (*Rpc.rpc_of_string vm_name;*)
         ] in
      let del = Rpc.to_string rpc_del in
      let del_vm = `String (irmin_task ^ del ^ "\"}") in
      let uri = (Uri.of_string ("http://irmin/update/jitsu/request/" ^ vm_name ^ "/request")) in
      (*http_post c stack uri del_vm;*)
      C.log c (sprintf "LOW LOAD -> delete replica: %d" avg_objreq) (* For debugging *)
    );
    (*else C.log c (sprintf "Objects requested in last 5s: %d" (!obj_count - !oid));*) (* For debugging *)
    *)
    C.log c (sprintf "avg objects/sec: %d" avg_objreq); *)

  let rec replicate_timer n c =
    Time.sleep n >>= fun () -> 
    replicate_flag := true;
    (*C.log c(sprintf "flag %b" !replicate_flag);*)
    replicate_timer n c

  (* START *)
  let start c fs stack n0 =
    let vm_name = Macaddr.to_string (N0.mac n0) in
    let find_port = List.exists (fun x -> vm_name = x) irmin_2 in
    if find_port = true then (irmin_port := 8081 ) else (irmin_port := 8080);
    Hashtbl.add host_table "irmin"
      (fun ~port -> `TCP (Ipaddr.of_string_exn irmin_ip, !irmin_port));
    let uri = (Uri.of_string ("http://irmin/read/jitsu/" ^ vm_name ^ "/initial_xs")) in 
    lwt initial_xs = http_get c stack uri in
    Hashtbl.add host_table "initial_xs"     
      (fun ~port -> `TCP (Ipaddr.of_string_exn initial_xs, 8080));
    
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
            incr obj_count;
            Lwt.catch (fun () -> 
                read_fs path >>= fun body ->                
                (*log := !log ^ "200 OK;";
                C.log c (sprintf "After serving:\n%s" !log);*)
                (* Extract body length here *)
                H.respond_string ~status:`OK ~body ()   
              )  (fun exn ->
                (*log := !log ^ "404 Not Found;";
                C.log c (sprintf "Error entry:\n%s" !log);*)
                H.respond_not_found ()
              )
             (* >>=  fun () ->
                lwt body_size = body |> Cohttp_lwt_body.to_string >|= fun body ->
                String.length body in
                C.log_s c (sprintf "Body size %s" (string_of_int body_size)) 
                >>= fun () -> return()*)
        in
        (* HTTP callback *)
        let callback conn_id request body =
          t0 := Time.Monotonic.to_seconds(Time.Monotonic.time());
          (* Unsupported gmtime on Xen, issue #333
          let date = Clock.gmtime(Clock.time ()) in  (* day/month/year:hour:min:sec *)
          let (day, month, year, hour, min, sec) = (date.tm_mday, date.tm_mon, date.tm_year, date.tm_hour, date.tm_min, date.tm_sec) in  
          (sprintf "%d/%d/%d:%d:%d:%d" day month year hour min sec)
          *)
          (* Logging function *)
          let uri = Cohttp.Request.uri request in
          (*
          let meth = Cohttp.Code.string_of_method (Cohttp.Request.meth request) in
          let headers = Cohttp.Header.to_string (Cohttp.Request.headers request) in
          (*lwt body_size = body |> Cohttp_lwt_body.to_string >|= fun body ->
          String.length body in*)
          log := !log ^ (sprintf "%f" !t0) ^ (Uri.to_string uri) ^ meth ^ headers (*^ (string_of_int body_size)*); *)
          dispatcher (split_path uri)          
        in
        let conn_closed (_,conn_id) =
          let cid = Cohttp.Connection.to_string conn_id  in
          (*C.log c (sprintf "conn %s closed" cid); (* For debugging *) *)
          (*log_store := [|!log_store ^ log|];
          log := "";*) 
          ()
          in
        Conduit_mirage.with_tcp conduit stackv4 stack >>= fun conduit ->
        let spec = H.make ~conn_closed ~callback:callback () in
        Conduit_mirage.listen conduit (`TCP 80) (H.listen spec));
        (replicate_timer 5.0 c);
        (monitoring c stack vm_name 5.0);] (* Scale down monitor thread *)

end
