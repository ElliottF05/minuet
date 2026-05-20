exception Unix_exception of Core_unix.Error.t

val serve_connection : Core_unix.File_descr.t -> H1.Server_connection.request_handler -> unit

val serve : ?bind_addr:string -> int -> H1.Server_connection.request_handler -> unit

val request
  :  ?body:string
  -> Core_unix.sockaddr
  -> H1.Request.t
  -> (H1.Response.t * string, H1.Client_connection.error) Result.t