(** Cooperatively-scheduled socket primitives.

    Each function is a thin wrapper over the corresponding non-blocking 
    syscall: when waiting, it suspends the current task on fd readiness using 
    {!Executor}'s {!Executor.wait_readable} or {!Executor.wait_writable} rather 
    than blocking the OS thread, so these must be called from a task running 
    under {!Executor.start}.

    Where a function has meaningful expected failures it returns them via 
    [Result]; everything else propagates as [Unix_error] exceptions. *)

val listen : ?bind_addr:string -> int -> Core_unix.File_descr.t
(** [listen ?bind_addr port] creates a TCP listening socket. If [bind_addr] is 
    omitted, binds to all addresses. *)

val accept 
  :  Core_unix.File_descr.t 
  -> Core_unix.File_descr.t * Core_unix.sockaddr
(** Cooperatively block until a connection arrives, then return the
    accepted socket (already set non-blocking) and the peer address. *)

val connect 
  :  Core_unix.sockaddr 
  -> (Core_unix.File_descr.t, Core_unix.Error.t) Result.t
(** [connect addr] opens a TCP connection to [addr], cooperatively blocking when
    needed. The returned fd is non-blocking. *)

val read
  :  Core_unix.File_descr.t
  -> Bytes.t
  -> ?pos:int
  -> ?len:int
  -> unit
  -> (int, Core_unix.Error.t) Result.t
(** [read fd buf ?pos ?len ()] reads up to [len] bytes into [buf] starting at 
    [pos], cooperatively blocking when waiting for IO. Returns the number of 
    bytes read; [0] means the peer closed its write side. *)

val read_bigstring
  :  Core_unix.File_descr.t
  -> Bigstringaf.t
  -> ?off:int
  -> ?len:int
  -> unit
  -> (int, Core_unix.Error.t) Result.t
(** Like {!read} but into a bigstring. *)

val write
  :  Core_unix.File_descr.t
  -> Bytes.t
  -> ?pos:int
  -> ?len:int
  -> unit
  -> (int, Core_unix.Error.t) Result.t
(** [write fd buf ?pos ?len ()] writes up to [len] bytes from [buf] starting at 
    [pos], cooperatively blocking when waiting for IO. The returned count may be
    less than [len] (partial write); the caller is responsible for looping. *)

val write_bigstring
  :  Core_unix.File_descr.t
  -> Bigstringaf.t
  -> ?off:int
  -> ?len:int
  -> unit
  -> (int, Core_unix.Error.t) Result.t
(** Like {!write} but from a bigstring. *)

val writev
  :  Core_unix.File_descr.t
  -> Bigstringaf.t Faraday.iovec list
  -> (int, Core_unix.Error.t) Result.t
(** [writev fd iovecs] writes a list of bigstring iovecs one-by-one, 
    cooperatively blocking when waiting for IO. Returns the total bytes written;
    on a partial write of an iovec, stops and returns the partial total. *)

val close : Core_unix.File_descr.t -> unit
(** Close [fd]. *)

val shutdown : Core_unix.File_descr.t -> mode:Core_unix.shutdown_command -> unit
(** [shutdown fd ~mode] half-closes one direction of the connection.

    Use [SHUTDOWN_SEND] and [SHUTDOWN_RECEIVE] for the writing and reading 
    directions of the fd respectively. *)