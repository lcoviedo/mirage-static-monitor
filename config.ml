open Mirage

let mode =
  try match String.lowercase (Unix.getenv "FS") with
    | "fat" -> `Fat
    | _     -> `Crunch
  with Not_found ->
    `Crunch

let fat_ro dir =
  kv_ro_of_fs (fat_of_files ~dir ())

let fs = match mode with
  | `Fat    -> fat_ro "./htdocs"
  | `Crunch -> crunch "./htdocs"

let net =
  try match Sys.getenv "NET" with
    | "direct" -> `Direct
    | "socket" -> `Socket
    | _        -> `Direct
  with Not_found -> `Direct

let dhcp =
  try match Sys.getenv "DHCP" with
    | "0" -> false
    | _  -> true
  with Not_found -> true

let stack console =
  match net, dhcp with
  | `Direct, true  -> direct_stackv4_with_dhcp console tap0
  | `Direct, false -> direct_stackv4_with_default_ipv4 console tap0
  | `Socket, _     -> socket_stackv4 console [Ipaddr.V4.any]

let main =
  foreign "Dispatch.Main" (console @-> kv_ro @-> stackv4 @-> network @-> job)

let () =
  add_to_ocamlfind_libraries ["re.str";"mirage-http";"rpclib";];
  add_to_opam_packages ["re";"mirage-http";];
  let sv4 = stack default_console in
  let job =  [ main $ default_console $ fs $ sv4 $ tap0 ] in
  register "static" job
