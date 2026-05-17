open Core
module Mutex = Error_checking_mutex
module Thread = Core_thread

type 'a future_state = 
  | Pending of (unit, unit) continuation Queue.t
  | Resolved of 'a
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
  | Resolved _ -> failwith "unreachable"

let resolve t v = 
  t.state <- Resolved v