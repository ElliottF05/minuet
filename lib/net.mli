val listen : ?bind_addr:string -> int -> Core_unix.File_descr.t

val accept : Core_unix.File_descr.t -> Core_unix.File_descr.t * Core_unix.sockaddr

val connect : Core_unix.sockaddr -> (Core_unix.File_descr.t, Core_unix.Error.t) Result.t

val read
  :  Core_unix.File_descr.t
  -> Bytes.t
  -> ?pos:int
  -> ?len:int
  -> unit
  -> (int, Core_unix.Error.t) Result.t

val read_bigstring
  :  Core_unix.File_descr.t
  -> Bigstringaf.t
  -> ?off:int
  -> ?len:int
  -> unit
  -> (int, Core_unix.Error.t) Result.t

val write
  :  Core_unix.File_descr.t
  -> Bytes.t
  -> ?pos:int
  -> ?len:int
  -> unit
  -> (int, Core_unix.Error.t) Result.t

val write_bigstring
  :  Core_unix.File_descr.t
  -> Bigstringaf.t
  -> ?off:int
  -> ?len:int
  -> unit
  -> (int, Core_unix.Error.t) Result.t

val writev
  :  Core_unix.File_descr.t
  -> Bigstringaf.t Faraday.iovec list
  -> (int, Core_unix.Error.t) Result.t

val close : Core_unix.File_descr.t -> unit

val shutdown : Core_unix.File_descr.t -> mode:Core_unix.shutdown_command -> unit