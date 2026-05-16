open Core
open Minuet.Executor

(* Test 1: Basic yield interleaving — tasks should interleave, not run sequentially *)
let test_interleaving () =
  start (fun () ->
    spawn (fun () ->
      prerr_endline "[A] start";
      yield ();
      prerr_endline "[A] after first yield";
      yield ();
      prerr_endline "[A] end";
    );
    spawn (fun () ->
      prerr_endline "[B] start";
      yield ();
      prerr_endline "[B] end";
    );
    prerr_endline "[main] spawned A and B, returning"
  )

(* Test 2: Recursive spawning — tasks that themselves spawn children *)
let test_recursive_spawn () =
  start (fun () ->
    spawn (fun () ->
      prerr_endline "[parent] start";
      spawn (fun () ->
        prerr_endline "[child] start";
        yield ();
        prerr_endline "[child] end";
      );
      prerr_endline "[parent] spawned child";
      yield ();
      prerr_endline "[parent] end";
    )
  )

(* Test 3: Many tasks — stress test the queue *)
let test_many_tasks () =
  start (fun () ->
    List.iter (List.range 0 10) ~f:(fun i ->
      spawn (fun () ->
        Printf.eprintf "[task %d] start\n" i;
        yield ();
        Printf.eprintf "[task %d] end\n" i;
      )
    )
  )

(* Test 4: Exception isolation — one task failing shouldn't kill others *)
let test_exception () =
  start (fun () ->
    spawn (fun () ->
      prerr_endline "[stable] start";
      yield ();
      prerr_endline "[stable] end";
    );
    spawn (fun () ->
      prerr_endline "[faulty] about to raise";
      failwith "something went wrong";
    );
    spawn (fun () ->
      prerr_endline "[also stable] running after faulty task";
    )
  )

let () =
  prerr_endline "=== test_interleaving ===";
  test_interleaving ();
  prerr_endline "=== test_recursive_spawn ===";
  test_recursive_spawn ();
  prerr_endline "=== test_many_tasks ===";
  test_many_tasks ();
  prerr_endline "=== test_exception ===";
  test_exception ()