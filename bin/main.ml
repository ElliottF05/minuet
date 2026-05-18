open Core
open Minuet.Executor

let test_interleaving () =
  start (fun () ->
    ignore (spawn (fun () ->
      prerr_endline "[A] start";
      yield ();
      prerr_endline "[A] after yield";
      yield ();
      prerr_endline "[A] end";
    ));
    ignore (spawn (fun () ->
      prerr_endline "[B] start";
      yield ();
      prerr_endline "[B] end";
    ));
    prerr_endline "[main] spawned A and B, returning"
  )

let test_many_tasks () =
  start (fun () ->
    List.iter (List.range 0 10) ~f:(fun i ->
      ignore (spawn (fun () ->
        Printf.eprintf "[task %d] start\n" i;
        yield ();
        Printf.eprintf "[task %d] end\n" i;
      ))
    )
  )

let test_exception () =
  start (fun () ->
    ignore (spawn (fun () ->
      prerr_endline "[stable] start";
      yield ();
      prerr_endline "[stable] end";
    ));
    ignore (spawn (fun () ->
      prerr_endline "[faulty] about to raise";
      failwith "something went wrong";
    ));
    ignore (spawn (fun () ->
      prerr_endline "[also stable] running";
    ))
  )

(* Test: basic await — waiter should resume after producer finishes *)
let test_await_basic () =
  start (fun () ->
    let f = spawn (fun () -> 
      prerr_endline "[producer] working";
      yield ();
      prerr_endline "[producer] done";
      42
    ) in
    ignore (spawn (fun () ->
      prerr_endline "[consumer] waiting on future";
      let result = await_exn f in
      Printf.eprintf "[consumer] got %d\n" result
    ))
  )

(* Test: multiple waiters on the same future *)
let test_await_multiple_waiters () =
  start (fun () ->
    let f = spawn (fun () ->
      yield ();
      100
    ) in
    List.iter (List.range 0 3) ~f:(fun i ->
      ignore (spawn (fun () ->
        let result = await_exn f in
        Printf.eprintf "[waiter %d] got %d\n" i result
      ))
    )
  )

(* Test: chained awaits — task C awaits B which awaits A *)
let test_await_chained () =
  start (fun () ->
    let a = spawn (fun () ->
      prerr_endline "[A] computing";
      yield ();
      1
    ) in
    let b = spawn (fun () ->
      let a_result = await_exn a in
      Printf.eprintf "[B] got A=%d, computing\n" a_result;
      a_result + 1
    ) in
    ignore (spawn (fun () ->
      let b_result = await_exn b in
      Printf.eprintf "[C] got B=%d\n" b_result
    ))
  )

(* Test: spawn_blocking — cooperative task awaits an OS thread result *)
let test_await_blocking () =
  start (fun () ->
    let f = spawn_blocking (fun () ->
      prerr_endline "[thread] sleeping";
      ignore (Core_unix.nanosleep 0.2);
      prerr_endline "[thread] done";
      99
    ) in
    ignore (spawn (fun () ->
      prerr_endline "[consumer] waiting on blocking task";
      let result = await_exn f in
      Printf.eprintf "[consumer] got %d\n" result
    ))
  )

let test_await_multiple_blocking () =
  start (fun () ->
    let f1 = spawn_blocking (fun () ->
      prerr_endline "[thread 1] sleeping";
      ignore (Core_unix.nanosleep 0.3);
      prerr_endline "[thread 1] done";
      42
    ) in
    let f2 = spawn_blocking (fun () ->
      prerr_endline "[thread 2] sleeping";
      ignore (Core_unix.nanosleep 0.1);
      prerr_endline "[thread 2] done";
      99
    ) in
    let f3 = spawn_blocking (fun () ->
      prerr_endline "[thread 3] sleeping";
      ignore (Core_unix.nanosleep 0.2);
      prerr_endline "[thread 3] done";
      7
    ) in
    ignore (spawn (fun () ->
      prerr_endline "[consumer] waiting on all blocking tasks";
      let r1 = await_exn f1 in
      let r2 = await_exn f2 in
      let r3 = await_exn f3 in
      Printf.eprintf "[consumer] got %d, %d, %d\n" r1 r2 r3
    ))
  )

let test_async_sleep_basic () =
  start (fun () ->
    ignore (spawn (fun () ->
      prerr_endline "[task] before sleep";
      ignore (await (sleep 0.2));
      prerr_endline "[task] after sleep (should be ~0.2s later)"
    ))
  )

let test_async_sleep_interleaved () =
  start (fun () ->
    ignore (spawn (fun () ->
      prerr_endline "[A] before sleep";
      ignore (await (sleep 0.3));
      prerr_endline "[A] after sleep (should be ~0.3s later)"
    ));
    ignore (spawn (fun () ->
      prerr_endline "[B] before sleep";
      ignore (await (sleep 0.1));
      prerr_endline "[B] after sleep (should be ~0.1s later, before A)"
    ));
    prerr_endline "[main] spawned A and B"
  )

(* B and C finish before A — verifies ordering *)
let test_async_sleep_ordering () =
  start (fun () ->
    ignore (spawn (fun () ->
      ignore (await (sleep 0.3));
      prerr_endline "[A] done (should print 3rd)"
    ));
    ignore (spawn (fun () ->
      ignore (await (sleep 0.1));
      prerr_endline "[B] done (should print 1st)"
    ));
    ignore (spawn (fun () ->
      ignore (await (sleep 0.2));
      prerr_endline "[C] done (should print 2nd)"
    ))
  )

(* sleep then do work, interleaved with a non-sleeping task *)
let test_async_sleep_with_yielding_task () =
  start (fun () ->
    ignore (spawn (fun () ->
      prerr_endline "[sleeper] going to sleep";
      ignore (await (sleep 0.2));
      prerr_endline "[sleeper] woke up"
    ));
    ignore (spawn (fun () ->
      prerr_endline "[worker] step 1";
      yield ();
      prerr_endline "[worker] step 2";
      yield ();
      prerr_endline "[worker] step 3"
    ))
  )

(* chained sleeps — total should be ~0.3s *)
let test_async_sleep_chained () =
  start (fun () ->
    ignore (spawn (fun () ->
      prerr_endline "[task] step 1";
      ignore (await (sleep 0.1));
      prerr_endline "[task] step 2 (~0.1s)";
      ignore (await (sleep 0.1));
      prerr_endline "[task] step 3 (~0.2s)";
      ignore (await (sleep 0.1));
      prerr_endline "[task] step 4 (~0.3s)"
    ))
  )

(* sleep and blocking thread together *)
let test_async_sleep_with_blocking () =
  start (fun () ->
    ignore (spawn (fun () ->
      prerr_endline "[sleeper] sleeping 0.2s";
      ignore (await (sleep 0.2));
      prerr_endline "[sleeper] done"
    ));
    let f = spawn_blocking (fun () ->
      prerr_endline "[thread] sleeping 0.1s";
      ignore (Core_unix.nanosleep 0.1);
      prerr_endline "[thread] done";
      42
    ) in
    ignore (spawn (fun () ->
      let result = await_exn f in
      Printf.eprintf "[consumer] blocking result: %d (should arrive before sleeper)\n" result
    ))
  )

let () =
  prerr_endline "=== test_interleaving ===";
  test_interleaving ();
  prerr_endline "=== test_many_tasks ===";
  test_many_tasks ();
  prerr_endline "=== test_exception ===";
  test_exception ();
  prerr_endline "=== test_await_basic ===";
  test_await_basic ();
  prerr_endline "=== test_await_multiple_waiters ===";
  test_await_multiple_waiters ();
  prerr_endline "=== test_await_chained ===";
  test_await_chained ();
  prerr_endline "=== test_await_blocking ===";
  test_await_blocking ();
  prerr_endline "=== test_await_multiple_blocking ===";
  test_await_multiple_blocking ();
  prerr_endline "=== test_async_sleep_basic ===";
  test_async_sleep_basic ();
  prerr_endline "=== test_async_sleep_interleaved ===";
  test_async_sleep_interleaved ();
  prerr_endline "=== test_async_sleep_ordering ===";
  test_async_sleep_ordering ();
  prerr_endline "=== test_async_sleep_with_yielding_task ===";
  test_async_sleep_with_yielding_task ();
  prerr_endline "=== test_async_sleep_chained ===";
  test_async_sleep_chained ();
  prerr_endline "=== test_async_sleep_with_blocking ===";
  test_async_sleep_with_blocking ();