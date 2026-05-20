open Core
open Minuet.Executor
module Net = Minuet.Net
module Http = Minuet.Http

(* --- shared helpers (raw TCP) --- *)

let rec send_all fd buf ~pos ~len =
  if len = 0 then ()
  else
    match Net.write fd buf ~pos ~len () with
    | Ok 0 -> failwith "send_all: write returned 0"
    | Ok n -> send_all fd buf ~pos:(pos + n) ~len:(len - n)
    | Error err -> failwith ("send_all: " ^ Caml_unix.error_message err)

let echo_conn conn_fd label =
  let buf = Bytes.create 1024 in
  let rec loop () =
    match Net.read conn_fd buf ~pos:0 ~len:1024 () with
    | Ok 0 -> Printf.printf "[%s] peer closed\n" label
    | Ok n ->
        Printf.printf "[%s] echoing %d bytes\n" label n;
        send_all conn_fd buf ~pos:0 ~len:n;
        loop ()
    | Error err -> Printf.printf "[%s] read error: %s\n" label (Caml_unix.error_message err)
  in
  loop ();
  Net.close conn_fd

let echo_client port label msg =
  let addr = Core_unix.ADDR_INET (Core_unix.Inet_addr.localhost, port) in
  match Net.connect addr with
  | Error err -> Printf.printf "[%s] connect failed: %s\n" label (Caml_unix.error_message err)
  | Ok fd ->
      send_all fd (Bytes.of_string msg) ~pos:0 ~len:(String.length msg);
      let buf = Bytes.create 1024 in
      (match Net.read fd buf ~pos:0 ~len:1024 () with
       | Ok n ->
           Printf.printf "[%s] received echo: %S\n" label
             (Bytes.to_string (Bytes.sub buf ~pos:0 ~len:n))
       | Error err -> Printf.printf "[%s] read error: %s\n" label (Caml_unix.error_message err));
      Net.close fd

(* --- shared helpers (HTTP) --- *)

let echo_handler reqd =
  let req = H1.Reqd.request reqd in
  let body = Printf.sprintf "hello %s" req.H1.Request.target in
  let headers = H1.Headers.of_list [
    "content-length", Int.to_string (String.length body);
    "connection",     "close";
  ] in
  let response = H1.Response.create ~headers `OK in
  H1.Reqd.respond_with_string reqd response body

let latency_handler latency reqd =
  sleep latency;
  echo_handler reqd

let stress_handler reqd =
  let lat = 0.005 +. Random.float 0.045 in
  sleep lat;
  echo_handler reqd

let make_get path =
  let headers = H1.Headers.of_list [
    "host",       "localhost";
    "connection", "close";
  ] in
  H1.Request.create ~headers `GET path

let run_test_server port n handler =
  let listen_fd = Net.listen port in
  spawn (fun () ->
    let conns = List.init n ~f:(fun _ ->
      let conn_fd, _ = Net.accept listen_fd in
      spawn (fun () -> Http.serve_connection conn_fd handler))
    in
    Net.close listen_fd;
    List.iter conns ~f:join_exn)

(* --- executor: yield / spawn / interleaving --- *)

let%expect_test "interleaving" =
  start (fun () ->
    ignore (spawn (fun () ->
      print_endline "[A] start";
      yield ();
      print_endline "[A] after yield";
      yield ();
      print_endline "[A] end"));
    ignore (spawn (fun () ->
      print_endline "[B] start";
      yield ();
      print_endline "[B] end"));
    print_endline "[main] spawned A and B, returning");
  [%expect {|
    [main] spawned A and B, returning
    [A] start
    [B] start
    [A] after yield
    [B] end
    [A] end
    |}]

let%expect_test "many tasks" =
  start (fun () ->
    List.iter (List.range 0 10) ~f:(fun i ->
      ignore (spawn (fun () ->
        Printf.printf "[task %d] start\n" i;
        yield ();
        Printf.printf "[task %d] end\n" i))));
  [%expect {|
    [task 0] start
    [task 1] start
    [task 2] start
    [task 3] start
    [task 4] start
    [task 5] start
    [task 6] start
    [task 7] start
    [task 8] start
    [task 9] start
    [task 0] end
    [task 1] end
    [task 2] end
    [task 3] end
    [task 4] end
    [task 5] end
    [task 6] end
    [task 7] end
    [task 8] end
    [task 9] end
    |}]

let%expect_test "exception in spawned task is isolated" =
  start (fun () ->
    ignore (spawn (fun () ->
      print_endline "[stable] start";
      yield ();
      print_endline "[stable] end"));
    ignore (spawn (fun () ->
      print_endline "[faulty] about to raise";
      failwith "something went wrong"));
    ignore (spawn (fun () ->
      print_endline "[also stable] running")));
  [%expect {|
    [stable] start
    [faulty] about to raise
    [also stable] running
    [stable] end
    |}]

(* --- executor: join --- *)

let%expect_test "join: basic" =
  start (fun () ->
    let f = spawn (fun () ->
      print_endline "[producer] working";
      yield ();
      print_endline "[producer] done";
      42)
    in
    ignore (spawn (fun () ->
      print_endline "[consumer] waiting on future";
      let result = join_exn f in
      Printf.printf "[consumer] got %d\n" result)));
  [%expect {|
    [producer] working
    [consumer] waiting on future
    [producer] done
    [consumer] got 42
    |}]

let%expect_test "join: multiple waiters on same handle" =
  start (fun () ->
    let f = spawn (fun () -> yield (); 100) in
    List.iter (List.range 0 3) ~f:(fun i ->
      ignore (spawn (fun () ->
        let result = join_exn f in
        Printf.printf "[waiter %d] got %d\n" i result))));
  [%expect {|
    [waiter 0] got 100
    [waiter 1] got 100
    [waiter 2] got 100
    |}]

let%expect_test "join: chained" =
  start (fun () ->
    let a = spawn (fun () ->
      print_endline "[A] computing";
      yield ();
      1)
    in
    let b = spawn (fun () ->
      let a_result = join_exn a in
      Printf.printf "[B] got A=%d, computing\n" a_result;
      a_result + 1)
    in
    ignore (spawn (fun () ->
      let b_result = join_exn b in
      Printf.printf "[C] got B=%d\n" b_result)));
  [%expect {|
    [A] computing
    [B] got A=1, computing
    [C] got B=2
    |}]

(* --- executor: spawn_blocking --- *)

let%expect_test "spawn_blocking: cooperative task joins an OS-thread result" =
  start (fun () ->
    let f = spawn_blocking (fun () ->
      print_endline "[thread] sleeping";
      ignore (Core_unix.nanosleep 0.2);
      print_endline "[thread] done";
      99)
    in
    ignore (spawn (fun () ->
      print_endline "[consumer] waiting on blocking task";
      let result = join_exn f in
      Printf.printf "[consumer] got %d\n" result)));
  [%expect {|
    [consumer] waiting on blocking task
    [thread] sleeping
    [thread] done
    [consumer] got 99
    |}]

(* OS thread *start* ordering is non-deterministic, so we don't snapshot
   the per-thread prints — just verify the consumer received each thread's
   return value correctly. *)
let%expect_test "spawn_blocking: multiple threads" =
  start (fun () ->
    let f1 = spawn_blocking (fun () -> ignore (Core_unix.nanosleep 0.3); 42) in
    let f2 = spawn_blocking (fun () -> ignore (Core_unix.nanosleep 0.1); 99) in
    let f3 = spawn_blocking (fun () -> ignore (Core_unix.nanosleep 0.2); 7) in
    ignore (spawn (fun () ->
      let r1 = join_exn f1 in
      let r2 = join_exn f2 in
      let r3 = join_exn f3 in
      Printf.printf "r1=%d r2=%d r3=%d\n" r1 r2 r3)));
  [%expect {| r1=42 r2=99 r3=7 |}]

(* --- executor: sleep --- *)

let%expect_test "sleep: basic" =
  start (fun () ->
    ignore (spawn (fun () ->
      print_endline "[task] before sleep";
      sleep 0.2;
      print_endline "[task] after sleep (~0.2s later)")));
  [%expect {|
    [task] before sleep
    [task] after sleep (~0.2s later)
    |}]

let%expect_test "sleep: interleaved" =
  start (fun () ->
    ignore (spawn (fun () ->
      print_endline "[A] before sleep";
      sleep 0.3;
      print_endline "[A] after sleep (~0.3s later)"));
    ignore (spawn (fun () ->
      print_endline "[B] before sleep";
      sleep 0.1;
      print_endline "[B] after sleep (~0.1s later, before A)"));
    print_endline "[main] spawned A and B");
  [%expect {|
    [main] spawned A and B
    [A] before sleep
    [B] before sleep
    [B] after sleep (~0.1s later, before A)
    [A] after sleep (~0.3s later)
    |}]

let%expect_test "sleep: distinct durations fire in duration order" =
  start (fun () ->
    ignore (spawn (fun () -> sleep 0.3; print_endline "[A] done (3rd)"));
    ignore (spawn (fun () -> sleep 0.1; print_endline "[B] done (1st)"));
    ignore (spawn (fun () -> sleep 0.2; print_endline "[C] done (2nd)")));
  [%expect {|
    [B] done (1st)
    [C] done (2nd)
    [A] done (3rd)
    |}]

let%expect_test "sleep: with yielding task" =
  start (fun () ->
    ignore (spawn (fun () ->
      print_endline "[sleeper] going to sleep";
      sleep 0.2;
      print_endline "[sleeper] woke up"));
    ignore (spawn (fun () ->
      print_endline "[worker] step 1";
      yield ();
      print_endline "[worker] step 2";
      yield ();
      print_endline "[worker] step 3")));
  [%expect {|
    [sleeper] going to sleep
    [worker] step 1
    [worker] step 2
    [worker] step 3
    [sleeper] woke up
    |}]

let%expect_test "sleep: chained" =
  start (fun () ->
    ignore (spawn (fun () ->
      print_endline "[task] step 1";
      sleep 0.1;
      print_endline "[task] step 2 (~0.1s)";
      sleep 0.1;
      print_endline "[task] step 3 (~0.2s)";
      sleep 0.1;
      print_endline "[task] step 4 (~0.3s)")));
  [%expect {|
    [task] step 1
    [task] step 2 (~0.1s)
    [task] step 3 (~0.2s)
    [task] step 4 (~0.3s)
    |}]

let%expect_test "sleep + blocking thread interleave" =
  start (fun () ->
    ignore (spawn (fun () ->
      print_endline "[sleeper] sleeping 0.2s";
      sleep 0.2;
      print_endline "[sleeper] done"));
    let f = spawn_blocking (fun () ->
      print_endline "[thread] sleeping 0.1s";
      ignore (Core_unix.nanosleep 0.1);
      print_endline "[thread] done";
      42)
    in
    ignore (spawn (fun () ->
      let result = join_exn f in
      Printf.printf "[consumer] blocking result: %d (before sleeper)\n" result)));
  [%expect {|
    [sleeper] sleeping 0.2s
    [thread] sleeping 0.1s
    [thread] done
    [consumer] blocking result: 42 (before sleeper)
    [sleeper] done
    |}]

(* --- reactor: wait_readable --- *)

let%expect_test "wait_readable on a pipe" =
  start (fun () ->
    let r, w = Core_unix.pipe () in
    ignore (spawn (fun () ->
      print_endline "[reader] waiting until readable";
      wait_readable r;
      let buf = Bytes.create 64 in
      let n = Core_unix.read r ~buf in
      Printf.printf "[reader] read value %s\n" (Bytes.to_string (Bytes.sub buf ~pos:0 ~len:n))));
    sleep 0.2;
    print_endline "[writer] about to write (should arrive before reader receives)";
    ignore (Core_unix.write w ~buf:(Bytes.of_string "hello")));
  [%expect {|
    [reader] waiting until readable
    [writer] about to write (should arrive before reader receives)
    [reader] read value hello
    |}]

(* --- raw TCP echo --- *)

let%expect_test "tcp echo: one client, one round-trip" =
  start (fun () ->
    let port = 8131 in
    let listen_fd = Net.listen port in
    let server = spawn (fun () ->
      let conn_fd, _addr = Net.accept listen_fd in
      Net.close listen_fd;
      echo_conn conn_fd "server")
    in
    let client = spawn (fun () -> echo_client port "client" "hello, minuet") in
    join_exn client;
    join_exn server);
  [%expect {|
    [server] echoing 13 bytes
    [client] received echo: "hello, minuet"
    [server] peer closed
    |}]

(* Concurrent TCP connections interleave non-deterministically at the print
   level, so this variant collects per-client results and just verifies
   that each client got back exactly what it sent. *)
let%expect_test "tcp echo: 3 clients via accept loop" =
  let n = 3 in
  let port = 8132 in
  let results = Array.create ~len:n "" in
  let silent_echo_conn conn_fd =
    let buf = Bytes.create 1024 in
    let rec loop () =
      match Net.read conn_fd buf ~pos:0 ~len:1024 () with
      | Ok 0 -> ()
      | Ok n -> send_all conn_fd buf ~pos:0 ~len:n; loop ()
      | Error _ -> ()
    in
    loop ();
    Net.close conn_fd
  in
  let silent_echo_client i =
    let msg = Printf.sprintf "message from client %d" i in
    let addr = Core_unix.ADDR_INET (Core_unix.Inet_addr.localhost, port) in
    (match Net.connect addr with
     | Error _ -> ()
     | Ok fd ->
         send_all fd (Bytes.of_string msg) ~pos:0 ~len:(String.length msg);
         let buf = Bytes.create 1024 in
         (match Net.read fd buf ~pos:0 ~len:1024 () with
          | Ok len -> results.(i) <- Bytes.to_string (Bytes.sub buf ~pos:0 ~len)
          | Error _ -> ());
         Net.close fd)
  in
  start (fun () ->
    let listen_fd = Net.listen port in
    let server = spawn (fun () ->
      let handlers = List.init n ~f:(fun _ ->
        let conn_fd, _addr = Net.accept listen_fd in
        spawn (fun () -> silent_echo_conn conn_fd))
      in
      Net.close listen_fd;
      List.iter handlers ~f:join_exn)
    in
    let clients = List.init n ~f:(fun i -> spawn (fun () -> silent_echo_client i)) in
    join_exn server;
    List.iter clients ~f:join_exn);
  let all_correct =
    Array.foldi results ~init:true ~f:(fun i acc r ->
      acc && String.equal r (Printf.sprintf "message from client %d" i))
  in
  Printf.printf "all clients got expected echo: %b\n" all_correct;
  [%expect {| all clients got expected echo: true |}]

(* --- HTTP (h1 adapter) --- *)

let%expect_test "http: single round-trip" =
  start (fun () ->
    let port = 8201 in
    let server = run_test_server port 1 echo_handler in
    let addr = Core_unix.ADDR_INET (Core_unix.Inet_addr.localhost, port) in
    (match Http.request addr (make_get "/hello") with
     | Ok (_resp, body) -> Printf.printf "[client] got: %S\n" body
     | Error _          -> print_endline "[client] request failed");
    join_exn server);
  [%expect {| [client] got: "hello /hello" |}]

let%expect_test "http: 5 sequential requests, one server" =
  start (fun () ->
    let port = 8202 in
    let n = 5 in
    let server = run_test_server port n echo_handler in
    let addr = Core_unix.ADDR_INET (Core_unix.Inet_addr.localhost, port) in
    List.iter (List.range 0 n) ~f:(fun i ->
      match Http.request addr (make_get (Printf.sprintf "/%d" i)) with
      | Ok (_resp, body) -> Printf.printf "[client] req %d: %S\n" i body
      | Error _          -> Printf.printf "[client] req %d failed\n" i);
    join_exn server);
  [%expect {|
    [client] req 0: "hello /0"
    [client] req 1: "hello /1"
    [client] req 2: "hello /2"
    [client] req 3: "hello /3"
    [client] req 4: "hello /4"
    |}]

(* N parallel client tasks against a server that sleeps [latency]s per response.
   If concurrency works, total elapsed should be ~latency, not n*latency.
   Snapshot booleans rather than the raw elapsed value. *)
let%expect_test "http: concurrent requests overlap" =
  start (fun () ->
    let port = 8203 in
    let n = 10 in
    let latency = 0.2 in
    let server = run_test_server port n (latency_handler latency) in
    let addr = Core_unix.ADDR_INET (Core_unix.Inet_addr.localhost, port) in
    let t0 = Core_unix.gettimeofday () in
    let clients = List.init n ~f:(fun i ->
      spawn (fun () -> Http.request addr (make_get (Printf.sprintf "/%d" i))))
    in
    let oks = List.count clients ~f:(fun h -> Result.is_ok (join_exn h)) in
    let elapsed = Core_unix.gettimeofday () -. t0 in
    let bound = latency *. 1.1 in
    Printf.printf "all_ok: %b\n" (oks = n);
    Printf.printf "elapsed < %.2fs: %b\n" bound Float.(elapsed < bound);
    join_exn server);
  [%expect {|
    all_ok: true
    elapsed < 0.22s: true
    |}]

(* Sustained N-in-flight: worker pool pulls from shared counter (single-threaded
   → safe). Snapshot booleans: every request succeeded, no errors, and the peak
   in-flight count really did reach the configured concurrency. *)
let%expect_test "http: stress run sustains high concurrency" =
  start (fun () ->
    let port = 8204 in
    let total = 4000 in
    let concurrency = 100 in
    let server = run_test_server port total stress_handler in
    let addr = Core_unix.ADDR_INET (Core_unix.Inet_addr.localhost, port) in
    let remaining = ref total in
    let oks = ref 0 in
    let errs = ref 0 in
    let in_flight = ref 0 in
    let peak = ref 0 in
    let t0 = Core_unix.gettimeofday () in
    let workers = List.init concurrency ~f:(fun w ->
      spawn (fun () ->
        let rec loop () =
          if !remaining > 0 then begin
            decr remaining;
            incr in_flight;
            peak := max !peak !in_flight;
            (match Http.request addr (make_get (Printf.sprintf "/w%d" w)) with
             | Ok _    -> incr oks
             | Error _ -> incr errs);
            decr in_flight;
            loop ()
          end
        in
        loop ()))
    in
    List.iter workers ~f:join_exn;
    join_exn server;
    let elapsed = Core_unix.gettimeofday () -. t0 in
    (* Theoretical floor is [total * avg_latency / concurrency]; bound at 1.5x
       leaves headroom for scheduling overhead while still catching a real
       regression in concurrency. *)
    let avg_latency = (0.005 +. 0.050) /. 2.0 in
    let floor = Float.of_int total *. avg_latency /. Float.of_int concurrency in
    let bound = floor *. 1.5 in
    Printf.printf "all_ok: %b\n" (!oks = total);
    Printf.printf "errs: %d\n" !errs;
    Printf.printf "peak_inflight >= %d: %b\n" (concurrency / 2) (!peak >= concurrency / 2);
    Printf.printf "elapsed < %.2fs: %b\n" bound Float.(elapsed < bound));
  [%expect {|
    all_ok: true
    errs: 0
    peak_inflight >= 50: true
    elapsed < 1.65s: true
    |}]
 