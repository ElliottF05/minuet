(** Cooperatively-scheduled HTTP/1.1 server and client, driven by the [h1] state
    machine over {!Net} primitives.

    Serves as a proof of concept that {!Executor} is general enough to back a 
    real protocol library: [h1] does no IO itself, this module supplies the IO 
    loop, and everything runs cooperatively on a single thread. All operations 
    must be called under {!Executor.start}. *)

exception Unix_exception of Core_unix.Error.t
(** Wraps a {!Net} error so that it can be reported into h1's [`Exn] variant. *)

val serve_connection 
  :  Core_unix.File_descr.t 
  -> H1.Server_connection.request_handler 
  -> unit
(** [serve_connection conn_fd handler] drives one accepted HTTP connection to 
    completion, cooperatively blocking when waiting: paired read and write 
    loops, joined, followed by [Net.close].  Used internally by {!serve}; 
    exposed for custom accept loops. *)

val serve 
  :  ?bind_addr:string 
  -> int 
  -> H1.Server_connection.request_handler 
  -> unit
(** [serve ?bind_addr port handler] runs an HTTP/1.1 server on [port], spawning 
    a cooperative connection task per accepted connection. Never returns. *)

val request
  :  ?body:string
  -> Core_unix.sockaddr
  -> H1.Request.t
  -> (H1.Response.t * string, H1.Client_connection.error) Result.t
(** [request ?body addr req] sends [req] over a fresh TCP connection to [addr], 
    optionally including [body] as the request body, and returns the response 
    together with the accumulated response body. Cooperatively blocks while 
    waiting on the network.

    The caller is responsible for setting [Content-Length] on [req] when sending
    a body. *)