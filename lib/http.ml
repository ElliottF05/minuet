open Core

exception Unix_exception of Core_unix.Error.t

(** Serve one accepted HTTP connection to completion: paired read/write loops,
join both, close the fd. *)
let serve_connection connection_fd request_handler =
  let config = H1.Config.default in
  let connection = H1.Server_connection.create ~config request_handler in
  let read_buffer = Bigstringaf.create config.read_buffer_size in

  let rec read_loop () =
    match H1.Server_connection.next_read_operation connection with
    | `Read ->
        begin match Net.read_bigstring connection_fd read_buffer () with
        | Ok 0 -> ignore (H1.Server_connection.read_eof connection read_buffer ~off:0 ~len:0)
        | Ok bytes_available ->
            let rec consume off remaining =
              if remaining > 0 then begin
                let bytes_consumed = H1.Server_connection.read connection read_buffer ~off ~len:remaining in
                if bytes_consumed = 0 then () else consume (off + bytes_consumed) (remaining - bytes_consumed)
              end
            in
            consume 0 bytes_available
        | Error e ->
            H1.Server_connection.report_exn connection (Unix_exception e)
        end;
        read_loop ()
    | `Yield ->
        Executor.suspend (fun wakeup_thunk ->
          H1.Server_connection.yield_reader connection wakeup_thunk
        );
        read_loop ()
    | `Close ->
        Net.shutdown connection_fd ~mode:Core_unix.SHUTDOWN_RECEIVE
    | `Upgrade ->
        prerr_endline "HTTP upgrade requested but not supported, closing connection";
        Net.shutdown connection_fd ~mode:Core_unix.SHUTDOWN_RECEIVE
  in

  let rec write_loop () =
    match H1.Server_connection.next_write_operation connection with
    | `Write io_vectors ->
        begin match Net.writev connection_fd io_vectors with
        | Ok bytes_written -> H1.Server_connection.report_write_result connection (`Ok bytes_written)
        | Error _e -> H1.Server_connection.report_write_result connection `Closed
        end;
        write_loop ()
    | `Yield ->
        Executor.suspend (fun wakeup_thunk ->
          H1.Server_connection.yield_writer connection wakeup_thunk
        );
        write_loop ()
    | `Close _ ->
        Net.shutdown connection_fd ~mode:Core_unix.SHUTDOWN_SEND
    | `Upgrade ->
        prerr_endline "HTTP upgrade requested but not supported, closing connection";
        Net.shutdown connection_fd ~mode:Core_unix.SHUTDOWN_SEND
  in

  let reader = Executor.spawn read_loop in
  let writer = Executor.spawn write_loop in
  Executor.join_exn reader;
  Executor.join_exn writer;
  Net.close connection_fd


(** Starts an HTTP/1.1 server on, accepting connections in a loop.
Cooperatively yields to other tasks while waiting for connections or IO.
Does not return. *)
let serve ?bind_addr port request_handler =
  let listen_fd = Net.listen ?bind_addr port in
  let rec accept_loop () =
    let connection_fd, _client_addr = Net.accept listen_fd in
    ignore (Executor.spawn (fun () -> serve_connection connection_fd request_handler));
    accept_loop ()
  in
  accept_loop ()


let request ?body addr req = 
  match Net.connect addr with 
  | Error e -> Error (`Exn (Unix_exception e))
  | Ok connection_fd -> 
      let response_opt = ref (Error (`Exn (Failure "unreachable: response never started"))) in
      let config = H1.Config.default in
      let read_buffer = Bigstringaf.create config.read_buffer_size in
      let response_body_buffer = Buffer.create config.response_body_buffer_size in

      let error_handler err = 
        response_opt := Error err
      in
      let response_handler response body = 
        let rec read_body () = 
          H1.Body.Reader.schedule_read body 
            ~on_read:(fun bigstring ~off ~len -> 
              Buffer.add_string response_body_buffer (Bigstringaf.substring bigstring ~off ~len);
              read_body ()
            )
            ~on_eof:(fun () -> 
              response_opt := Ok (response, Buffer.contents response_body_buffer)
            );
        in
        read_body ()
      in

      let request_body, connection = H1.Client_connection.request ~config req ~error_handler ~response_handler in
      Option.iter body ~f:(H1.Body.Writer.write_string request_body);
      H1.Body.Writer.close request_body;

      let rec read_loop () = 
        match H1.Client_connection.next_read_operation connection with 
        | `Read -> 
            begin match Net.read_bigstring connection_fd read_buffer () with 
            | Ok 0 -> ignore (H1.Client_connection.read_eof connection read_buffer ~off:0 ~len:0)
            | Ok bytes_available -> 
                let rec consume off remaining = 
                  if remaining > 0 then begin
                    let bytes_consumed = H1.Client_connection.read connection read_buffer ~off ~len:remaining in
                    if bytes_consumed = 0 then () else consume (off + bytes_consumed) (remaining - bytes_consumed)
                  end
                in
                consume 0 bytes_available
            | Error e -> 
                H1.Client_connection.report_exn connection (Unix_exception e)
            end;
            read_loop ()
        | `Close -> 
            Net.shutdown connection_fd ~mode:Core_unix.SHUTDOWN_RECEIVE
      in

      let rec write_loop () = 
        match H1.Client_connection.next_write_operation connection with 
        | `Write io_vectors -> 
            begin match Net.writev connection_fd io_vectors with 
            | Ok bytes_written -> H1.Client_connection.report_write_result connection (`Ok bytes_written)
            | Error _e -> H1.Client_connection.report_write_result connection `Closed
            end;
            write_loop ()
        | `Yield -> 
            Executor.suspend (fun wakeup_thunk ->
              H1.Client_connection.yield_writer connection wakeup_thunk
            );
            write_loop ()
        | `Close _ ->
            Net.shutdown connection_fd ~mode:Core_unix.SHUTDOWN_SEND
      in

      let reader = Executor.spawn read_loop in
      let writer = Executor.spawn write_loop in
      Executor.join_exn reader;
      Executor.join_exn writer;
      Net.close connection_fd;

      !response_opt