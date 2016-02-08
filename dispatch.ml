open Lwt
open Printf
open V1_LWT
open OS
open Fractal_client

let red fmt    = sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = sprintf ("\027[36m"^^fmt^^"\027[m")

let rt = ref 0.0
let t_req = ref 0.0
let k = ref 0
let stats_array = Array.make 10 (0.0, 0, "")
let uri_req = (Uri.of_string ("http://irmin/update/jitsu/request/"))
let uri_dataset = (Uri.of_string ("http://irmin:8080/update/jitsu/exp/"))
let replicate_flag = ref false
let high_load = 1000  (* 1000 requests per second - High threshold value *)
let low_load = 100  (* 100 requests per second rate - Low threshold value *)
let irmin_ip = "127.0.0.1"  (* Location of irmin store *)
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

(** Move http_get *)
  (* Conduit connection helper *) 
  let conduit_conn stack =
    lwt conduit = Conduit_mirage.with_tcp conduit stackv4 stack in
      let res = Resolver_mirage.static host_table in
      let ctx = HTTP.ctx res conduit in 
      Lwt.return (conduit,ctx)

  let http_get c stack uri =
    lwt (conduit, ctx) = conduit_conn stack in
    C.log_s c (sprintf "Fetching %s:" (Uri.to_string uri)) >>= fun () -> (* debugging *)
    HTTP.get ~ctx uri >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >>= fun body ->
      (*let _ = C.log_s c (sprintf "Response: %s" (body)) in*) (* debugging *)
      let str = Re_str.replace_first (Re_str.regexp ".*\\[\"") "" body in
      let str_value = Re_str.replace_first (Re_str.regexp "\"\\].*") "" str in
      C.log_s c (sprintf "Value: %s" str_value) >>= fun () -> (* debugging *)
      Lwt.return(str_value)

  (* Monitoring function *)
  let rec monitoring spec stack t n c =  (* n equals 10 which is the lenght of rps array*)
  (* get requests per second and response times *)
  if !k < (int_of_float n) then (
    t_req := Clock.time();
    Time.sleep 1.0 >>= fun () ->
		let stats = (H.get_stats spec) in (* get_stats function from modified cohttp *)
		  let (failed, rps, active, rt_list) = stats in
			let rt_rpc = Rpc.Enum [
				  Rpc.rpc_of_string (List.fold_right (fun (x) acc -> (string_of_float x)^" ;"^acc) rt_list "")
				] in
			let rt = Rpc.to_string rt_rpc in
    stats_array.(!k) <- (!t_req,rps,rt);
    incr k;
    let _ =
    (* Scale out *)
      if (rps >= high_load && !replicate_flag = true) then (
        replicate t >>= fun () ->
        replicate_flag := false;
        C.log_s c (sprintf "CREATE REPLICA........")  (* debugging *)
      )
    (* Scale back *)
      else if (rps <= low_load && !replicate_flag = true) then (
        C.log c (sprintf "DELETE REPLICA!: %d" rps);
			  die t >>= fun () ->
			  Lwt.return (replicate_flag := false) (** Fail if http_post hangs out **)
      )
		(* Halt event *) (**TODO -> Enable halt with correct conditions*)
		(*
		  else if (rps < high_load && rps > (*low_load*) 1 && rt >= 0.0001) then (
			  halt t >>= fun () ->
			  C.log_s c (sprintf "HALT EVENT -> stop sending new traffic");
		  )
		*)
      else (
			  C.log_s c (sprintf "debug")
      )
    in
    monitoring spec stack t n c
  )
  else ( (* Post statistics results *)
		k := 0; (** TODO -> Fix Uri for results*)
		lwt x = create_irmin_client (module S:V1_LWT.STACKV4 with type t = 'a) stack uri_dataset in
		let _ =
			post_results x stats_array in
    monitoring spec stack t n c
  )
 
  let rec replicate_timer n c =
    Time.sleep n >>= fun () -> 
    replicate_flag := true;
    replicate_timer n c

  (* START *)
  let start c fs stack n0 =
    let vm_name = Macaddr.to_string (N0.mac n0) in
    let find_port = List.exists (fun x -> vm_name = x) irmin_2 in
    if find_port = true then (irmin_port := 8081 ) else (irmin_port := 8080);
    Hashtbl.add host_table "irmin"
      (fun ~port -> `TCP (Ipaddr.of_string_exn irmin_ip, !irmin_port)); (** enable initial_xs *)
    let uri = (Uri.of_string ("http://irmin/update/read/jitsu/" ^ vm_name ^ "/initial_xs")) in (** double check uri *)
    lwt initial_xs = http_get c stack uri in
    Hashtbl.add host_table "initial_xs"     
      (fun ~port -> `TCP (Ipaddr.of_string_exn initial_xs, 8080));
    lwt t = create_irmin_client (module S:V1_LWT.STACKV4 with type t = 'a) stack uri_req in (* initialise t*)
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
          let uri = Cohttp.Request.uri request in
          dispatcher (split_path uri)
        in
        let conn_closed (_,conn_id) (result) = (* result is added to comply with modified cohttp library *)
          let cid = Cohttp.Connection.to_string conn_id in
          (*C.log c (sprintf "conns closed: %s" cid);*)				
					()
          in
        Conduit_mirage.with_tcp conduit stackv4 stack >>= fun conduit ->
        let spec = H.make ~conn_closed ~callback:callback () in
        Lwt.join[( 
				  Conduit_mirage.listen conduit (`TCP 80) (H.listen spec));
          (replicate_timer 10.0 c); (** Change timer *)
          (monitoring spec stack t 10.0 c); (* Scale events thread *)
				]
end
