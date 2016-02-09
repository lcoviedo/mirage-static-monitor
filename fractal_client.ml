open Lwt
open Printf

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
    printf ("Posting in path %s") (Uri.to_string t.uri); (* debugging *)
    Lwt.return ()

let replicate t =
  t_req := Clock.time();
  printf "\n\nCreate replica";
  let rpc_add = Rpc.Enum [
      Rpc.rpc_of_string "add_vm";
      Rpc.rpc_of_string (sprintf "%f" !t_req);
    ]   in
  let add = Rpc.to_string rpc_add in
  let add_vm = `String (irmin_task ^ add ^ "\"}") in
  (*http_post t add_vm*)
  lwt x = Cohttp_lwt_body.to_string (add_vm) in
  Lwt.return (printf "%s" x)

let halt t =
  t_req := Clock.time();
  printf "\n\nHalt event";
  let rpc_halt = Rpc.Enum [
      Rpc.rpc_of_string "halt_vm";
      Rpc.rpc_of_string (sprintf "%f" !t_req);
    ] in
  let halt_vm = `String (irmin_task ^ (Rpc.to_string rpc_halt) ^ "\"}") in
  (*http_post t halt_vm*)
  lwt x = Cohttp_lwt_body.to_string (halt_vm) in
  Lwt.return (printf "%s" x)

let die t =
  t_req := Clock.time();
  printf "\n\nLow load";
  let rpc_del = Rpc.Enum [
      Rpc.rpc_of_string "del_vm";
      Rpc.rpc_of_string (sprintf "%f" !t_req);
    ] in
  let del = Rpc.to_string rpc_del in
  let del_vm = `String (irmin_task ^ del ^ "\"}") in
  (*http_post t del_vm*)
  lwt x = Cohttp_lwt_body.to_string (del_vm) in
  Lwt.return (printf "%s" x)

let post_results t task stats_array = (* post_results must wait ~10 secs to report results *)
  t_req := Clock.time();
  printf "\n\nResults";
  let rpc_results = Rpc.Enum [
      Rpc.rpc_of_string (Array.fold_right (fun (x, y, z) acc ->
        (string_of_float x)^" "^(sprintf "%d" y)^" "^z^";"^acc) stats_array "")
    ] in
  let rpc_string = Rpc.to_string rpc_results in
  let results = `String (task ^ rpc_string ^ "\"}") in
  (*http_post t results*)
  lwt x = Cohttp_lwt_body.to_string (results) in
  Lwt.return (printf "%s" x)
