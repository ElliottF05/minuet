include Core
include Effect
include Effect.Deep
module Mutex = Error_checking_mutex
module Thread = Core_thread

(* TODO: only spawn, spawn_blocking, and start should be externally accessible, everything
else is a private implementation detail *)

(* ready queue, run loop, spawn, yield *)


(* --- types and effects --- *)

type _ Effect.t += Spawn : (unit -> unit) -> unit Effect.t
type _ Effect.t += Yield : unit Effect.t

type task = 
  | Fresh of (unit -> unit)
  | Suspended of (unit, unit) continuation


(* --- private members and functions --- *)

let ready_queue = Queue.create ()
let ready_queue_mutex = Mutex.create ()

let yield () = 
  perform Yield

let enqueue t = 
  Mutex.lock ready_queue_mutex;
  Queue.enqueue ready_queue t;
  Mutex.unlock ready_queue_mutex

let rec dispatch_next () = 
  Mutex.lock ready_queue_mutex;
  let task = Queue.dequeue ready_queue in
  Mutex.unlock ready_queue_mutex;
  match task with 
  | Some (Fresh f) -> run f
  | Some (Suspended k) -> continue k ()
  | None -> ()

and run f = 
  match f () with 
  | () -> dispatch_next ()
  | exception e -> print_endline (Exn.to_string e); dispatch_next ()
  | effect (Spawn f), k -> enqueue (Fresh f); continue k ()
  | effect Yield, k -> enqueue (Suspended k); dispatch_next ()


(* --- public functions --- *)

let spawn f = 
  let future = Future.create () in
  let f' = fun () -> 
    let result = f () in
    Mutex.lock future.mutex;
    let waiters = Future.get_waiters future in
    Future.resolve future result;
    Queue.iter waiters ~f:(fun k -> enqueue (Suspended k));
    Mutex.unlock future.mutex
  in
  perform (Spawn f');
  future

let spawn_blocking f = 
  let future = Future.create () in
  let _thread = Thread.create (fun () ->
    let result = f () in
    Mutex.lock future.mutex;
    let waiters = Future.get_waiters future in
    Future.resolve future result;
    Queue.iter waiters ~f:(fun k -> enqueue (Suspended k));
    Mutex.unlock future.mutex
  ) () in
  future

let start main = 
  run main