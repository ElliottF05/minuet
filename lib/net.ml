open Core
open Executor


(* --- config/init --- *)

(* writing to a socket whose peer has closed raises SIGPIPE which by default
terminates this process. ignoring it allows us to handle it as EPIPE *)
let () = Core.Signal.ignore Core.Signal.pipe

(* --- server side --- *)

(** If bind_addr is None then binds to all addresses *)
let listen ?bind_addr port = 
  let listen_fd = Core_unix.socket ~domain:Core_unix.PF_INET ~kind:Core_unix.SOCK_STREAM ~protocol:0 () in
  Core_unix.set_nonblock listen_fd;
  Core_unix.setsockopt listen_fd Core_unix.SO_REUSEADDR true;
  let bind = match bind_addr with 
  | Some s -> Core_unix.Inet_addr.of_string s
  | None -> Core_unix.Inet_addr.bind_any
  in
  let socket_addr = Core_unix.ADDR_INET (bind, port) in
  Core_unix.bind listen_fd ~addr:socket_addr;
  Core_unix.listen listen_fd ~backlog:128;
  listen_fd

let rec accept listen_fd = 
  match Core_unix.accept listen_fd with 
  | (connection_fd, addr) -> Core_unix.set_nonblock connection_fd; (connection_fd, addr)
  | exception Core_unix.Unix_error ((Core_unix.EINTR | Core_unix.ECONNABORTED), _, _) -> accept listen_fd
  | exception Core_unix.Unix_error ((Core_unix.EWOULDBLOCK | Core_unix.EAGAIN), _, _) -> 
      wait_readable listen_fd;
      accept listen_fd


(* --- client side --- *)

(** Note: [addr] must be an IPv4 address *)
let connect addr = 
  let connection_fd = Core_unix.socket ~domain:Core_unix.PF_INET ~kind:Core_unix.SOCK_STREAM ~protocol:0 () in
  Core_unix.set_nonblock connection_fd;
  let expected = function
    | Core_unix.ECONNREFUSED | Core_unix.ETIMEDOUT | Core_unix.ECONNRESET | Core_unix.ENETUNREACH 
    | Core_unix.EHOSTUNREACH | Core_unix.ENETDOWN -> true
    | _ -> false
  in
  match Core_unix.connect connection_fd ~addr with 
  | () -> Ok connection_fd
  | exception Core_unix.Unix_error ((Core_unix.EINPROGRESS | Core_unix.EINTR), _, _) -> 
      wait_writable connection_fd;
      begin match Caml_unix.getsockopt_error connection_fd with 
      | None -> Ok connection_fd
      | Some err -> Core_unix.close connection_fd; Error err
      end
  | exception (Core_unix.Unix_error (err, _, _) as exn) ->
      Core_unix.close connection_fd;
      if expected err then Error err else raise exn


(* --- shared (both server and client) --- *)

(** Returns the number of bytes read (0 means EOF) *)
let rec read fd buf ?pos ?len () =
  match Core_unix.read fd ~buf ?pos ?len with 
  | n -> Ok n
  | exception Core_unix.Unix_error (Core_unix.EINTR, _, _) -> read fd buf ?pos ?len ()
  | exception Core_unix.Unix_error ((Core_unix.EWOULDBLOCK | Core_unix.EAGAIN), _, _) -> 
      wait_readable fd;
      read fd buf ?pos ?len ()
  | exception Core_unix.Unix_error ((Core_unix.ECONNRESET | Core_unix.ETIMEDOUT) as err, _, _) -> Error err

(** Returns the number of bytes read (0 means EOF) *)
let rec read_bigstring fd bigstring ?off ?len () = 
  match Bigstring_unix.read fd bigstring ?off ?len with
    | n -> Ok n
  | exception Core_unix.Unix_error (Core_unix.EINTR, _, _) -> read_bigstring fd bigstring ?off ?len ()
  | exception Core_unix.Unix_error ((Core_unix.EWOULDBLOCK | Core_unix.EAGAIN), _, _) -> 
      wait_readable fd;
      read_bigstring fd bigstring ?off ?len ()
  | exception Core_unix.Unix_error ((Core_unix.ECONNRESET | Core_unix.ETIMEDOUT) as err, _, _) -> Error err

(** Returns the number of bytes written (might be less than ~len) *)
let rec write fd buf ?pos ?len () =
  match Core_unix.write fd ~buf ?pos ?len with 
  | n -> Ok n
  | exception Core_unix.Unix_error (Core_unix.EINTR, _, _) -> write fd buf ?pos ?len () 
  | exception Core_unix.Unix_error ((Core_unix.EWOULDBLOCK | Core_unix.EAGAIN), _, _) -> 
      wait_writable fd;
     write fd buf ?pos ?len ()
  | exception Core_unix.Unix_error ((Core_unix.ECONNRESET | Core_unix.ETIMEDOUT | Core_unix.EPIPE) as err, _, _) -> Error err

(** Returns the number of bytes written (might be less than ~len) *)
let rec write_bigstring fd bigstring ?off ?len () = 
  match Bigstring_unix.write fd bigstring ?off ?len with 
  | n -> Ok n
  | exception Core_unix.Unix_error (Core_unix.EINTR, _, _) -> write_bigstring fd bigstring ?off ?len () 
  | exception Core_unix.Unix_error ((Core_unix.EWOULDBLOCK | Core_unix.EAGAIN), _, _) -> 
      wait_writable fd;
      write_bigstring fd bigstring ?off ?len ()
  | exception Core_unix.Unix_error ((Core_unix.ECONNRESET | Core_unix.ETIMEDOUT | Core_unix.EPIPE) as err, _, _) -> Error err

(** Returns the number of bytes written *)
let writev fd iovecs =
  List.fold_until iovecs ~init:0
    ~f:(fun total iovec ->
      match write_bigstring fd iovec.Faraday.buffer ~off:iovec.off ~len:iovec.len () with
      | Ok n when n < iovec.len -> Stop (Ok (total + n))
      | Ok n -> Continue (total + n)
      | Error e -> Stop (Error e)
    )
    ~finish:(fun total -> Ok total)

let close fd = 
  Core_unix.close fd

(** SHUTDOWN_RECEIVE stops reading on this socket, SHUTDOWN_SEND stops writing *)
let shutdown fd ~mode =
  Core_unix.shutdown fd ~mode