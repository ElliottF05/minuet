open Core
open Effect
open Effect.Deep

(* ready queue, run loop, spawn, yield *)

type _ Effect.t += Spawn : (unit -> unit) -> unit t
type _ Effect.t += Yield: unit t

type task = 
  | Fresh of (unit -> unit)
  | Suspended of (unit, unit) continuation

let spawn f = 
  perform (Spawn f)

let yield () = 
  perform Yield

let start main = 
  let ready_queue = Queue.create () in
  let enqueue t = 
    Queue.enqueue ready_queue t
  in
  let rec run_next () = 
    match Queue.dequeue ready_queue with 
    | Some (Fresh f) -> execute f
    | Some (Suspended k) -> continue k ()
    | None -> ()
  and execute f = 
    match f () with 
    | () -> run_next ()
    | exception e -> print_endline (Exn.to_string e); run_next ()
    | effect (Spawn f), k -> enqueue (Fresh f); continue k ()
    | effect Yield, k -> enqueue (Suspended k); run_next ()
  in
  execute main