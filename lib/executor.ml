include Core
include Effect
include Effect.Deep
module Mutex = Stdlib.Mutex

(* ready queue, run loop, spawn, yield *)


(* --- types and effects --- *)

type _ Effect.t += Yield : unit Effect.t
type _ Effect.t += Await : 'a Future.t -> unit Effect.t

type task = 
  | Fresh of (unit -> unit)
  | Suspended of (unit, unit) continuation

type state = {
  ready_queue: task Queue.t;
  ready_queue_mutex: Mutex.t;
  blocking_counter: int Atomic.t;
  read_fds: Core_unix.File_descr.t list;
  write_fds: Core_unix.File_descr.t list;
  wakeups: (float * unit Future.t) Pairing_heap.t;
}


(* --- private members and functions --- *)

let notify_read, notify_write = Core_unix.pipe ()
let () = Core_unix.set_nonblock notify_read

let state = { 
  ready_queue = Queue.create ();
  ready_queue_mutex = Mutex.create ();
  blocking_counter = Atomic.make 0;
  read_fds = [notify_read];
  write_fds = [];
  wakeups = Pairing_heap.create ~cmp:(fun (t1, _) (t2, _) -> Float.compare t1 t2) ();
}

let enqueue t = 
  Mutex.lock state.ready_queue_mutex;
  Queue.enqueue state.ready_queue t;
  Mutex.unlock state.ready_queue_mutex

let resolve_future future result = 
  let waiters = 
    let w = Future.get_waiters future in
    future.state <- Future.Resolved result;
    w
  in
  Queue.iter waiters ~f:(fun k -> enqueue (Suspended k))

let rec dispatch_next () = 
  let rec resolve_expired_wakeups () = 
    match Pairing_heap.top state.wakeups with 
    | None -> ()
    | Some (wake_at, future) -> 
        if Float.(wake_at <= Core_unix.gettimeofday ()) then begin
          Pairing_heap.remove_top state.wakeups;
          resolve_future future (Ok ());
          resolve_expired_wakeups ()
        end
  in
  let get_timeout () = 
    match Pairing_heap.top state.wakeups with 
    | None -> `Never
    | Some (wake_at, future) ->  
        let delay = wake_at -. Core_unix.gettimeofday () in
        if Float.(delay <= 0.0) then
          `Immediately
        else 
          `After (Time_ns.Span.of_sec delay)
  in
  let drain_notify () = 
    let buf = Bytes.create 64 in
    let rec loop () = 
      match Core_unix.read notify_read ~buf ~pos:0 ~len:64 with 
      | _ -> loop ()
      | exception Core_unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> ()
      | exception exn -> raise exn
    in
    loop ()
  in
  let rec wait_for_task () = 
    match Queue.dequeue state.ready_queue with 
    | Some t -> Some t
    | None -> 
        if Atomic.get state.blocking_counter = 0 && Pairing_heap.is_empty state.wakeups then 
          None 
        else begin
          Mutex.unlock state.ready_queue_mutex; (* TODO: check why mutex needs to be released here *)
          ignore (Core_unix.select 
            ~read:state.read_fds ~write:state.write_fds ~except:[] 
            ~timeout:(get_timeout ())
          ());
          drain_notify ();
          resolve_expired_wakeups ();
          Mutex.lock state.ready_queue_mutex;
          wait_for_task ()
        end
  in

  resolve_expired_wakeups ();
  Mutex.lock state.ready_queue_mutex;
  let task = wait_for_task () in
  Mutex.unlock state.ready_queue_mutex;
  match task with 
  | Some (Fresh f) -> run f
  | Some (Suspended k) -> continue k ()
  | None -> ()

and run f = 
  match f () with 
  | () -> dispatch_next ()
  | effect Yield, k -> enqueue (Suspended k); dispatch_next ()
  | effect (Await future), k -> 
      begin match future.state with
      | Pending _ -> Future.add_waiter future k
      | Resolved _ -> enqueue (Suspended k)
      end;
      dispatch_next ()


(* --- public functions --- *)

let spawn f = 
  let future = Future.create () in
  let f' () = resolve_future future (Result.try_with f) in
  enqueue (Fresh f');
  future

let spawn_blocking f = 
  Atomic.incr state.blocking_counter;
  let future = Future.create () in
  let _thread = Caml_threads.Thread.create (fun () ->
    let result = Result.try_with (fun () -> f ()) in
    resolve_future future result;
    Atomic.decr state.blocking_counter;
    ignore (Core_unix.write notify_write ~buf:(Bytes.create 1) ~pos:0 ~len:1)
  ) () in
  future

let await future = 
  perform (Await future);
  Future.get_result future

let await_exn future = 
  Result.ok_exn (await future)

let yield () = 
  perform Yield

let sleep t = 
  let wake_at = Core_unix.gettimeofday () +. t in
  let future = Future.create () in
  Pairing_heap.add state.wakeups (wake_at, future);
  future

let start main = 
  run main