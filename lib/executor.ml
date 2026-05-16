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
  let rec dispatch_next () = 
    match Queue.dequeue ready_queue with 
    | Some (Fresh f) -> run f
    | Some (Suspended k) -> resume k
    | None -> ()
  and run f = 
    match f () with 
    | () -> dispatch_next ()
    | exception e -> print_endline (Exn.to_string e); dispatch_next ()
    | effect (Spawn f), k -> enqueue (Fresh f); resume k
    | effect Yield, k -> enqueue (Suspended k); dispatch_next ()
  and resume k = 
    continue k ()
  in
  run main