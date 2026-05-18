open Core

type 'a state = 
  | Pending of (unit, unit) continuation Queue.t
  | Resolved of ('a, exn) Result.t
type 'a t = {
  mutable state: 'a state
}

let create () = { state = Pending (Queue.create ()) }