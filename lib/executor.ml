open Core
open Effect
open Effect.Deep
module Mutex = Stdlib.Mutex

(* Notes and invariants: 
  - Threading: every task runs on the single executor thread; only spawn_blocking bodies run on other OS threads.
  - A blocking thread touches only blocking_completions (guarded by blocking_completions_mutex) and the wakeup_write channel. 
    It never touches the scheduler's internal ready_queue and other structures.
  - ready_queue / wakeups / future.state / blocking_counter are executor-thread-only and don't need synchronization.
*)

(* --- types and effects --- *)

type _ Effect.t += Yield : unit Effect.t
type _ Effect.t += Await : 'a Future.t -> unit Effect.t

type task = 
  | Fresh of (unit -> unit)
  | Suspended of (unit, unit) continuation

type state = {
  ready_queue: task Queue.t;
  timers: (float * unit Future.t) Pairing_heap.t;
  blocking_completions: (unit -> unit) Queue.t;
  blocking_completions_mutex: Mutex.t;
  mutable blocking_in_flight: int;
}


(* --- private members and functions --- *)

let wakeup_read, wakeup_write = Core_unix.pipe ()
let () = Core_unix.set_nonblock wakeup_read

let state = { 
  ready_queue = Queue.create ();
  timers = Pairing_heap.create ~cmp:(fun (t1, _) (t2, _) -> Float.compare t1 t2) ();
  blocking_in_flight = 0;
  blocking_completions = Queue.create ();
  blocking_completions_mutex = Mutex.create ();
}

let resolve_future future result = 
  match future.Future.state with 
  | Resolved _ -> failwith "future resolved twice"
  | Pending waiters ->
      future.state <- Future.Resolved result;
      Queue.iter waiters ~f:(fun k -> Queue.enqueue state.ready_queue (Suspended k))

let rec resolve_expired_wakeups () = 
  match Pairing_heap.top state.timers with 
  | None -> ()
  | Some (wake_at, future) -> 
      if Float.(wake_at <= Core_unix.gettimeofday ()) then begin
        Pairing_heap.remove_top state.timers;
        resolve_future future (Ok ());
        resolve_expired_wakeups ()
      end

let get_next_timeout () = 
  match Pairing_heap.top state.timers with 
  | None -> `Never
  | Some (wake_at, _future) ->  
      let delay = wake_at -. Core_unix.gettimeofday () in
      if Float.(delay <= 0.0) then
        `Immediately
      else 
        `After (Time_ns.Span.of_sec delay)

let drain_notify_channel () = 
  let buf = Bytes.create 64 in
  let rec loop () = 
    match Core_unix.read wakeup_read ~buf ~pos:0 ~len:64 with 
    | _ -> loop ()
    | exception Core_unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> ()
    | exception exn -> raise exn
  in
  loop ()

let drain_blocking_completions () = 
  Mutex.lock state.blocking_completions_mutex;
  let completions = Queue.to_list state.blocking_completions in
  Queue.clear state.blocking_completions;
  Mutex.unlock state.blocking_completions_mutex;
  List.iter completions ~f:(fun completion -> 
    completion ();
    state.blocking_in_flight <- state.blocking_in_flight - 1
  )

let rec wait_for_task () = 
  drain_blocking_completions ();
  resolve_expired_wakeups ();
  match Queue.dequeue state.ready_queue with 
  | Some task -> Some task
  | None ->
      let is_idle =  state.blocking_in_flight = 0 && Pairing_heap.is_empty state.timers in
      if is_idle then 
        None 
      else (
        ignore (Core_unix.select ~read:[wakeup_read] ~write:[] ~except:[] 
          ~timeout:(get_next_timeout ())
        ());
        drain_notify_channel ();
        wait_for_task ()
      )

let rec dispatch_next () = 
  match wait_for_task () with 
  | Some (Fresh f) -> run f
  | Some (Suspended k) -> continue k ()
  | None -> ()

and run f = 
  match f () with 
  | () -> dispatch_next ()
  | effect Yield, k -> Queue.enqueue state.ready_queue (Suspended k); dispatch_next ()
  | effect (Await future), k -> 
      begin match future.state with
      | Pending waiters -> Queue.enqueue waiters k
      | Resolved _ -> Queue.enqueue state.ready_queue (Suspended k)
      end;
      dispatch_next ()


(* --- public functions --- *)

let spawn f = 
  let future = Future.create () in
  let f' () = resolve_future future (Result.try_with f) in
  Queue.enqueue state.ready_queue (Fresh f');
  future

(** `f` runs on a separate OS thread, outside the scheduler: it must NOT
call `await` / `yield` / `sleep` / `spawn`. *)
let spawn_blocking f = 
  state.blocking_in_flight <- state.blocking_in_flight + 1;
  let future = Future.create () in
  let _thread = Caml_threads.Thread.create (fun () ->
    let result = Result.try_with (fun () -> f ()) in
    Mutex.lock state.blocking_completions_mutex;
    Queue.enqueue state.blocking_completions (fun () -> resolve_future future result);
    Mutex.unlock state.blocking_completions_mutex;
    ignore (Core_unix.write wakeup_write ~buf:(Bytes.create 1) ~pos:0 ~len:1)
  ) () in
  future

let await future = 
  perform (Await future);
  match future.state with 
  | Pending _ -> failwith "future still pending after being awaited"
  | Resolved v -> v

let await_exn future = 
  Result.ok_exn (await future)

let yield () = 
  perform Yield

let sleep t = 
  let wake_at = Core_unix.gettimeofday () +. t in
  let future = Future.create () in
  Pairing_heap.add state.timers (wake_at, future);
  future

let start main = 
  run main