open Lwt

let t_req = ref 0.0
let irmin_task = "{\"task\":{\"date\":\"0\",\"uid\":\"0\",\"owner\":\"\",\"messages\":[\"write\"]},\"params\":\""

let host_table =
  Hashtbl.create 3

(** *) (*It will need to add the irmin ip to the host table*)

(*type error
type response = [ `Ok | `Error of error ] *)
(** An invocation response. *)

module HTTP = Cohttp_mirage.Client

type t = {
    conduit_helper: (Conduit_mirage.t * HTTP.ctx);
    uri: Uri.t;
}

let create_irmin_client s stack uri =
  let stackv4 = Conduit_mirage.stackv4 s in 
  lwt conduit = Conduit_mirage.with_tcp Conduit_mirage.empty stackv4 stack in
  let res = Resolver_mirage.static host_table in
  let ctx = HTTP.ctx res conduit in
  Lwt.return { (* returns t which is t.conduit_helper and t.uri *)
    conduit_helper = (conduit,ctx);
    uri;
  }

let http_post t req =
    let (conduit, ctx) = t.conduit_helper in
    HTTP.post ~ctx ~body:req t.uri >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string req >>= fun body -> 
		Printf.printf ("Posting in path %s") (Uri.to_string t.uri); (* debugging *)
    Lwt.return ()

let replicate t =
  t_req := Clock.time();
  let rpc_add = Rpc.Enum [
      Rpc.rpc_of_string "add_vm";
      Rpc.rpc_of_string (Printf.sprintf "%f" !t_req);
    ]   in
  let add = Rpc.to_string rpc_add in
  let add_vm = `String (irmin_task ^ add ^ "\"}") in
  http_post t add_vm

let die t =
  t_req := Clock.time();
  let rpc_del = Rpc.Enum [
      Rpc.rpc_of_string "del_vm";
      Rpc.rpc_of_string (Printf.sprintf "%f" !t_req);
    ] in
  let del = Rpc.to_string rpc_del in
  let del_vm = `String (irmin_task ^ del ^ "\"}") in
  http_post t del_vm
