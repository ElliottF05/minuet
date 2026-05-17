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
      let result = await f in
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
        let result = await f in
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
      let a_result = await a in
      Printf.eprintf "[B] got A=%d, computing\n" a_result;
      a_result + 1
    ) in
    ignore (spawn (fun () ->
      let b_result = await b in
      Printf.eprintf "[C] got B=%d\n" b_result
    ))
  )

(* Test: spawn_blocking — cooperative task awaits an OS thread result *)
let test_await_blocking () =
  start (fun () ->
    let f = spawn_blocking (fun () ->
      prerr_endline "[thread] sleeping";
      ignore (Core_unix.nanosleep 0.1);
      prerr_endline "[thread] done";
      99
    ) in
    ignore (spawn (fun () ->
      prerr_endline "[consumer] waiting on blocking task";
      let result = await f in
      Printf.eprintf "[consumer] got %d\n" result
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
  test_await_blocking ()