open Core
module Mutex = Stdlib.Mutex
module Thread = Core_thread

type 'a future_state = 
  | Pending of (unit, unit) continuation Queue.t
  | Resolved of ('a, exn) Result.t
type 'a t = {
  mutable state: 'a future_state;
  mutex: Mutex.t
}

let create () = 
  { 
    state = Pending (Queue.create ()); 
    mutex = Mutex.create ();
  }

let get_waiters t = 
  match t.state with 
  | Pending waiters -> waiters
  | Resolved _ -> assert false

let get_result t = 
  match t.state with 
  | Pending _ -> assert false
  | Resolved v -> v

let add_waiter t k = 
  Queue.enqueue (get_waiters t) k