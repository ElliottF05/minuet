open Core
open Minuet

(* TODO: is it worth checking for eintr? *)

(* --- server side --- *)

let listen port = 
  let listen_fd = Core_unix.socket ~domain:Core_unix.PF_INET ~kind:Core_unix.SOCK_STREAM ~protocol:0 () in
  Core_unix.set_nonblock listen_fd;
  Core_unix.setsockopt listen_fd Core_unix.SO_REUSEADDR true;
  let socket_addr = Core_unix.ADDR_INET (Core_unix.Inet_addr.bind_any, port) in (* TODO: should i take bind address as param? *)
  Core_unix.bind listen_fd ~addr:socket_addr;
  Core_unix.listen listen_fd ~backlog:128;
  listen_fd

(* i assume this should be async since it shouldn't block waiting for new connections? *)
(* im not really sure about the implementation here... *)
let accept listen_fd = 
  let (connection_fd, addr) = Core_unix.accept listen_fd in
  Core_unix.set_nonblock connection_fd;
  (connection_fd, addr)


(* --- client side --- *)

(* should this be async? probably? *)
(** Note: `addr` must be an IPv4 address *)
let connect addr = 
  let connection_fd = Core_unix.socket ~domain:Core_unix.PF_INET ~kind:Core_unix.SOCK_STREAM ~protocol:0 () in
  Core_unix.set_nonblock connection_fd;
  Core_unix.connect connection_fd ~addr;
  connection_fd


(* --- shared (both server and client) --- *)

(* read/write (should be async, what should the signature be?) *)
let read fd buf ~pos ~len =
  Core_unix.read fd ~buf ~pos ~len

let write fd buf ~pos ~len = 
  Core_unix.write fd ~buf ~pos ~len

let close fd = 
  Core_unix.close fd