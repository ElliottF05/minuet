open Core

(* for now, start with Server_connection *)

exception Unix_exception of Core_unix.Error.t

let serve ?(bind_addr) port request_handler = 
  let listen_fd = Net.listen ?bind_addr port in
  let config = H1.Config.default in

  let rec connect_loop () = 
    let connection_fd, _client_addr = Net.accept listen_fd in
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

    ignore (Executor.spawn (fun () ->
      let reader = Executor.spawn read_loop in
      let writer = Executor.spawn write_loop in
      Executor.join_exn reader;
      Executor.join_exn writer;
      Net.close connection_fd
    ));
    connect_loop ()
  in

  connect_loop ()