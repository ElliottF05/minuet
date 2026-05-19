open Core

type 'a state = 
  | Pending of (('a, exn) Result.t, unit) continuation Queue.t (* a waiter is an joining task's continuation, resumed with the ('a, exn) result once known *)
  | Resolved of ('a, exn) Result.t
type 'a t = {
  mutable state: 'a state
}

let create () = { state = Pending (Queue.create ()) }