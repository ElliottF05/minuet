include Core
include Effect
include Effect.Deep
module Mutex = Stdlib.Mutex

(* TODO: only spawn, spawn_blocking, and start should be externally accessible, everything
else is a private implementation detail *)

(* ready queue, run loop, spawn, yield *)


(* --- types and effects --- *)

type _ Effect.t += Spawn : (unit -> unit) -> unit Effect.t
type _ Effect.t += Yield : unit Effect.t
type _ Effect.t += Await : 'a Future.t -> unit Effect.t

type task = 
  | Fresh of (unit -> unit)
  | Suspended of (unit, unit) continuation

type state = {
  ready_queue: task Queue.t;
  mutex: Mutex.t;
  condition: Condition.t;
  blocking_counter: int Atomic.t;
}


(* --- private members and functions --- *)

let state = { 
  ready_queue = Queue.create ();
  mutex = Mutex.create ();
  condition = Condition.create ();
  blocking_counter = Atomic.make 0;
}

let enqueue t = 
  Mutex.lock state.mutex;
  Queue.enqueue state.ready_queue t;
  Condition.signal state.condition;
  Mutex.unlock state.mutex

let rec dispatch_next () = 
  Mutex.lock state.mutex;
  let rec wait_for_task () = 
    match Queue.dequeue state.ready_queue with 
    | Some t -> Some t
    | None -> 
        if Atomic.get state.blocking_counter = 0 then None else begin
          Condition.wait state.condition state.mutex; 
          wait_for_task ()
        end
  in
  let task = wait_for_task () in
  Mutex.unlock state.mutex;
  match task with 
  | Some (Fresh f) -> run f
  | Some (Suspended k) -> continue k ()
  | None -> ()

and run f = 
  match f () with 
  | () -> dispatch_next ()
  | effect (Spawn f), k -> enqueue (Fresh f); continue k ()
  | effect Yield, k -> enqueue (Suspended k); dispatch_next ()
  | effect (Await future), k -> 
    Mutex.lock future.mutex;
    begin match future.state with
    | Pending _ -> Future.add_waiter future k
    | Resolved _ -> enqueue (Suspended k)
    end;
    Mutex.unlock future.mutex;
    dispatch_next ()


(* --- public functions --- *)

let spawn f = 
  let future = Future.create () in
  let f' = fun () -> 
    let result = Result.try_with (fun () -> f ()) in
    let waiters = 
      Mutex.lock future.mutex;
      let w = Future.get_waiters future in
      Future.resolve future result;
      Mutex.unlock future.mutex;
      w
    in
    Queue.iter waiters ~f:(fun k -> enqueue (Suspended k));
  in
  perform (Spawn f');
  future

let spawn_blocking f = 
  Atomic.incr state.blocking_counter;
  let future = Future.create () in
  let _thread = Caml_threads.Thread.create (fun () ->
    let result = Result.try_with (fun () -> f ()) in
    let waiters = 
      Mutex.lock future.mutex;
      let w = Future.get_waiters future in
      Future.resolve future result;
      Mutex.unlock future.mutex;
      w
    in
    Queue.iter waiters ~f:(fun k -> enqueue (Suspended k));
    Mutex.lock state.mutex;
    Atomic.decr state.blocking_counter;
    Condition.signal state.condition;
    Mutex.unlock state.mutex
  ) () in
  future

let await future = 
  perform (Await future);
  Future.get_result future

let await_exn future = 
  Result.ok_exn (await future)

let yield () = 
  perform Yield

let start main = 
  run main