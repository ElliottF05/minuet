open Core
open Effect
open Effect.Deep
module Mutex = Stdlib.Mutex

(* Notes and invariants: 
  - Threading: every task runs on the single executor thread; only spawn_blocking bodies run on other OS threads.
  - A blocking thread touches only blocking_completions (guarded by blocking_completions_mutex) and the wakeup_write channel. 
    It never touches the scheduler's internal ready_queue and other structures.
  - Everything except blocking_x in state is executor-thread-only and doesn't need synchronization.
*)

(* TODO: should i check for EINTR (or other errors) on unix syscalls? *)

(* --- types and effects --- *)

type _ Effect.t += Join : 'a Join_handle.t -> ('a, exn) Result.t Effect.t
type _ Effect.t += Suspend : ((unit, unit) continuation -> unit) -> unit Effect.t

type task = 
  | Fresh of (unit -> unit)
  | Continuation of (unit -> unit)

type state = {
  ready_queue: task Queue.t;
  timers: (float * (unit, unit) continuation) Pairing_heap.t;
  read_waiters: (Core_unix.File_descr.t, (unit, unit) continuation) Hashtbl.t;
  write_waiters: (Core_unix.File_descr.t, (unit, unit) continuation) Hashtbl.t;
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
  read_waiters = Hashtbl.create (module Core_unix.File_descr);
  write_waiters = Hashtbl.create (module Core_unix.File_descr);
  blocking_in_flight = 0;
  blocking_completions = Queue.create ();
  blocking_completions_mutex = Mutex.create ();
}

let enqueue_continuation k v = 
  Queue.enqueue state.ready_queue (Continuation (fun () -> continue k v))

let resolve_join_handle join_handle result = 
  match join_handle.Join_handle.state with 
  | Resolved _ -> failwith "unreachable: join_handle resolved twice"
  | Pending waiters ->
      join_handle.state <- Join_handle.Resolved result;
      Queue.iter waiters ~f:(fun k -> 
        enqueue_continuation k result
      )

let rec resolve_expired_wakeups () = 
  match Pairing_heap.top state.timers with 
  | None -> ()
  | Some (wake_at, k) -> 
      if Float.(wake_at <= Core_unix.gettimeofday ()) then begin
        Pairing_heap.remove_top state.timers;
        enqueue_continuation k ();
        resolve_expired_wakeups ()
      end

let is_idle () = 
  Queue.is_empty state.ready_queue
  && Pairing_heap.is_empty state.timers
  && Hashtbl.is_empty state.read_waiters
  && Hashtbl.is_empty state.write_waiters
  && state.blocking_in_flight = 0

let get_next_timeout () = 
  match Pairing_heap.top state.timers with 
  | None -> `Never
  | Some (wake_at, _k) ->  
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
    | exception Core_unix.Unix_error ((Core_unix.EAGAIN | Core_unix.EWOULDBLOCK), _, _) -> ()
    | exception exn -> raise exn
  in
  loop ()

let drain_blocking_completions () = 
  Mutex.lock state.blocking_completions_mutex;
  let completions = Queue.to_list state.blocking_completions in
  Queue.clear state.blocking_completions;
  Mutex.unlock state.blocking_completions_mutex;
  List.iter completions ~f:(fun thunk -> 
    thunk ();
    state.blocking_in_flight <- state.blocking_in_flight - 1
  )

let rec wait_for_task () = 
  drain_blocking_completions ();
  resolve_expired_wakeups ();
  match Queue.dequeue state.ready_queue with 
  | Some task -> Some task
  | None when is_idle () -> None
  | None ->
      let { read; write; except }: Core_unix.Select_fds.t = Core_unix.select 
        ~read:(wakeup_read :: Hashtbl.keys state.read_waiters) 
        ~write:(Hashtbl.keys state.write_waiters) 
        ~except:[] 
        ~timeout:(get_next_timeout ())
      () in
      List.iter read ~f:(fun fd -> 
        match Hashtbl.find_and_remove state.read_waiters fd with 
        | Some k -> enqueue_continuation k ()
        | None -> ()
      );
      List.iter write ~f:(fun fd -> 
        match Hashtbl.find_and_remove state.write_waiters fd with 
        | Some k -> enqueue_continuation k ()
        | None -> ()
      );
      drain_notify_channel ();
      wait_for_task ()

let rec dispatch_next () = 
  match wait_for_task () with 
  | Some (Fresh f) -> run f
  | Some (Continuation f) -> f ()
  | None -> ()

and run f = 
  match f () with 
  | () -> dispatch_next ()
  | effect (Suspend callback), k -> 
      callback k;
      dispatch_next ()
  | effect (Join join_handle), k -> 
      begin match join_handle.state with
      | Pending waiters -> Queue.enqueue waiters k
      | Resolved result -> enqueue_continuation k result
      end;
      dispatch_next ()


(* --- public functions --- *)

let spawn f = 
  let join_handle = Join_handle.create () in
  let run_and_resolve () = resolve_join_handle join_handle (Result.try_with f) in
  Queue.enqueue state.ready_queue (Fresh run_and_resolve);
  join_handle

(** `f` runs on a separate OS thread, outside the scheduler: it must NOT
call `join` / `yield` / `sleep` / `spawn`. *)
let spawn_blocking f = 
  state.blocking_in_flight <- state.blocking_in_flight + 1;
  let join_handle = Join_handle.create () in
  let _thread = Caml_threads.Thread.create (fun () ->
    let result = Result.try_with (fun () -> f ()) in
    Mutex.lock state.blocking_completions_mutex;
    Queue.enqueue state.blocking_completions (fun () -> resolve_join_handle join_handle result);
    Mutex.unlock state.blocking_completions_mutex;
    ignore (Core_unix.write wakeup_write ~buf:(Bytes.create 1) ~pos:0 ~len:1)
  ) () in
  join_handle

let join join_handle = 
  perform (Join join_handle)

let join_exn join_handle = 
  Result.ok_exn (join join_handle)

let yield () = 
  perform (Suspend (fun k -> enqueue_continuation k ()))

let sleep t = 
  let wake_at = Core_unix.gettimeofday () +. t in
  perform (Suspend (fun k -> Pairing_heap.add state.timers (wake_at, k)))

let wait_readable fd = 
  perform (Suspend (fun k -> Hashtbl.add_exn state.read_waiters ~key:fd ~data:k))

let wait_writable fd = 
  perform (Suspend (fun k -> Hashtbl.add_exn state.write_waiters ~key:fd ~data:k))

let start main = 
  run main