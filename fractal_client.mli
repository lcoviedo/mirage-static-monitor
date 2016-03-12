type t
(*type error
type response = [ `Ok | `Error of error ] *)
(** An invocation response. *)

val create_irmin_client: 
  (module V1_LWT.STACKV4 with type t = 'a) -> 
    'a -> Uri.t -> (string, (port:int -> Conduit.endp)) Hashtbl.t -> t Lwt.t

(*type handler = response -> unit Lwt.t*) (* return response lwt*)
(* An invocation response handler. Consumes a
response and returns a schedulable thread. *)

val replicate: t -> unit Lwt.t
(* [replicate t ()] asynchronously invokes Jitsu
to boot a new copy of this unikernel, calling [h]
with any response. *)

val halt: t -> unit Lwt.t
(* [halt t ()] asynchronously invokes Jitsu to
stop this unikernel receiving new flows, calling
[h] with any response. *)

val die: t -> unit Lwt.t
(* [die t ()] asynchronously invokes Jitsu to mark
this unikernel as dead and ready to be garbage
collected, calling [h] with any response*)

val post_results: t -> string -> (float * int * string) array -> unit Lwt.t
(* [post_results ()] asynchronously reports the
application metrics statistics to Jitsu *)
