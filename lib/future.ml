open Core

type 'a future_state = 
  | Pending of (unit, unit) continuation Queue.t
  | Resolved of ('a, exn) Result.t
type 'a t = {
  mutable state: 'a future_state;
}

let create () = 
  { 
    state = Pending (Queue.create ()); 
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