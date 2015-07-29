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
let i = ref 0
let delay_array = Array.make 64 0.0
let replicate_flag = ref true
let delete_flag = ref true                                                    (* Flag to replicate/destroy itself only once *)
let timeout_flag = ref false                                                     (* only possible to request after timeout expires *)
let burst_count = ref 0
let obj_count = ref 0
let oid1 = ref 0
let vm_name = ref ""

let avg_array = Array.make 128 (0.0, 0.0)
let avg_counter = ref 0
let exp_no = ref 0

let h_load = 0.00019                                                          (* High threshold value *)
let irmin_ip = "128.232.80.10" (*"10.0.0.1"*)                                                   (* Location of irmin store *)
                      

module Main (C:CONSOLE) (FS:KV_RO) (S:STACKV4) (N0:NETWORK) = struct

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
  
  (* Send request/posts to Irmin *)     
  let http_post c ctx req uri =    
    C.log_s c (sprintf "Posting in path %s" (Uri.to_string uri)) >>= fun () ->
    HTTP.post ~ctx ~body:req uri >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string req >>= fun body ->
    C.log_s c (sprintf "%s" body)
    
  (* Conduit connection helper *)
  let conduit_conn c stack req uri=
    Lwt.ignore_result (
      lwt conduit = Conduit_mirage.with_tcp Conduit_mirage.empty stackv4 stack in
        let res = Resolver_mirage.static irmin_store in
        let ctx = HTTP.ctx res conduit in
        http_post c ctx req uri)
     
  (* Monitor-Scale up based on request/reply times *)
  let scale_up c stack =  
    Lwt.return (
      t1 := Time.Monotonic.to_seconds(Time.Monotonic.time());       
      let delay = !t1 -. !t0 in
      delay_array.(!i) <- delay;
      (*C.log c (sprintf "delay = %f" delay);*)       (* For debugging *)                
      if !i < (Array.length delay_array - 1) 
        then incr i 
        else (
          i := 0;            
          let avg = 
            (Array.fold_right (+.) delay_array 0.0) /. float(Array.length delay_array) in
            t_avg := Clock.time();
            let avg_tuple = (!t_avg,avg) in 
              avg_array.(!avg_counter) <- avg_tuple;
              if !avg_counter < (Array.length avg_array - 1)
                then incr avg_counter
              else (
                avg_counter := 0;
                incr exp_no;
                C.log c (sprintf "***  RPC STRING COMPLETED %d  ***" !exp_no);                                       
                let rpc_avg_array = Rpc.Enum [
                    Rpc.rpc_of_string (Array.fold_right (fun (x, y) acc -> 
                        (string_of_float x)^" "^(sprintf "%f" y)^";"^acc ) avg_array "")
                    ] in
                let rpc_avg_string = Rpc.to_string rpc_avg_array in
                let avg_rpc = `String ("{\"params\":\"" ^ rpc_avg_string ^ "\"}") in
                let uri = (Uri.of_string ("http://irmin:8080/update/jitsu/exp/" ^ !vm_name ^ "/data" ^ (string_of_int !exp_no))) in
                conduit_conn c stack avg_rpc uri;
              );
          if (!replicate_flag = true && !timeout_flag = true) then ( 
            if avg > h_load then (                                                      (* above that ~ %80 cpu and likely to crash *) 
              (*C.log c (sprintf "CREATE REPLICA   Delay=%f  counter=%d" avg !burst_count); *)        (* For debugging *)
              incr burst_count;
              if !burst_count > 1 then (                                    (* avoid replica request on a single burst *)
                let rpc_add = Rpc.Enum [
                    Rpc.rpc_of_string "add";
                    Rpc.rpc_of_string !vm_name;
                    Rpc.rpc_of_string "static-web";
                  ] in
                let add = Rpc.to_string rpc_add in
                let add_vm = `String ("{\"params\":\"" ^ add ^ "\"}") in
                let uri = (Uri.of_string ("http://irmin:8080/update/jitsu/request/" ^ !vm_name ^ "/action")) in                                           
                conduit_conn c stack add_vm uri;
                replicate_flag := false; ) ) ) )
        )
        
  (* Monitor-Scale down based on objects requested *)        
  let rec scale_down c stack n =                                                      
      oid1 := !obj_count;                                                 (* if idle for 3 secs then trigger action *)
      Time.sleep n >>= fun () ->   
      if !oid1 = !obj_count && !timeout_flag = true then (
        let rpc_del = Rpc.Enum [
                    Rpc.rpc_of_string "delete";
                    Rpc.rpc_of_string !vm_name;
                  ] in
        let del = Rpc.to_string rpc_del in
        let delete_vm = `String ("{\"params\":\"" ^ del ^ "\"}") in
        let uri = (Uri.of_string ("http://irmin:8080/update/jitsu/request/" ^ !vm_name ^ "/action")) in
        conduit_conn c stack delete_vm uri;                                 
        delete_flag := false;
        (*C.log c (sprintf "LOW LOAD -> delete replica")*)                 (* For debugging *)           
        )
        (*else C.log c (
               sprintf "Objects requested in last 3s: %d" (!obj_count - !oid1))*);       (* For debugging *)
      scale_down c stack n 
          
  let request_timer n c stack =
    let _ = Time.sleep n >> Lwt.return(timeout_flag := true) >> Lwt.return(C.log c(sprintf "flag %b" !timeout_flag)) >> (scale_down c stack 5.0) in Lwt.return()
  (*let request_timer n c =
    let _ = Time.sleep n >> Lwt.return(timeout_flag := true) >> Lwt.return(C.log c(sprintf "flag %b" !timeout_flag)) in Lwt.return()*)
 
  let burst_timer n =
    let _ = Time.sleep n >> Lwt.return(burst_count := 0) in Lwt.return()

  (* START *)
  let start c fs stack n0 = 
   Lwt.join[(
    let _ = vm_name := Macaddr.to_string (N0.mac n0) in  
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
          incr obj_count;                                                                       
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
      let _ = scale_up c stack in                                        (* response time per object *)
      reply 
    in 
    let conn_closed (_,conn_id) =
      (*let cid = Cohttp.Connection.to_string conn_id  in
      C.log c (sprintf "conn %s closed" cid);                    (* For debugging *)
      *)
      () in   
      Conduit_mirage.with_tcp conduit stackv4 stack >>= fun conduit ->
      let spec = H.make ~conn_closed ~callback:callback () in
      Conduit_mirage.listen conduit (`TCP 80) (H.listen spec));                     
      (request_timer 5.0 c stack ); (**30.0*)         (* delay for replica/delete requests *) 
      (*request_timer 10.0 c >> (scale_down c stack 5.0);*)
      burst_timer 5.0;                                                              (* reset counter for incoming burst traffic *) 
      (*(scale_down c stack 3.0);*)]                                                    (* Scale down monitor thread *)
          
end
