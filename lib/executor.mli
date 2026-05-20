(** Single-threaded, effects-based cooperative scheduler.

    All "blocking" operations in this module ([join], [yield], [sleep],
    [wait_readable], [wait_writable], [suspend]) yield cooperatively to other 
    tasks while waiting rather than blocking the OS thread. The one exception is
    [spawn_blocking], whose body runs on its own OS thread. *)

type 'a join_handle
(** Opaque handle to a spawned task; produced by {!spawn} /
    {!spawn_blocking}, consumed by {!join} / {!join_exn}. *)

val spawn : (unit -> 'a) -> 'a join_handle
(** [spawn f] schedules [f] as a new cooperative task and returns a handle 
    resolving to its result. *)

val spawn_blocking : (unit -> 'a) -> 'a join_handle
(** [spawn_blocking f] runs [f] on a fresh OS thread, outside the scheduler. 
    [f] must not call any other function from this module. *)

val join : 'a join_handle -> ('a, exn) Result.t
(** [join h] cooperatively blocks until [h]'s task completes, returning its 
    result. *)

val join_exn : 'a join_handle -> 'a
(** Like {!join} but raises if the task ended with an exception. *)

val yield : unit -> unit
(** Reschedule the current task, letting other tasks run before continuing. *)

val sleep : float -> unit
(** [sleep t] cooperatively blocks the current task for at least [t] seconds. *)

val wait_readable : Core_unix.File_descr.t -> unit
(** Cooperatively block until [fd] becomes readable. *)

val wait_writable : Core_unix.File_descr.t -> unit
(** Cooperatively block until [fd] becomes writable. *)

val suspend : ((unit -> unit) -> unit) -> unit
(** [suspend register] parks the current task. [register] is invoked immediately
    with a one-shot [resume : unit -> unit] thunk; it should store [resume] 
    somewhere and arrange for it to be called when the task should wake. The 
    task remains suspended until [resume ()] is invoked, at which point 
    execution continues after the [suspend] call.

    Bridges {!Executor} to callback-style libraries that expose a "register
    a continuation" API. *)

val start : (unit -> unit) -> unit
(** [start main] runs [main] under the executor and returns once the executor 
    has no more running or waiting tasks. *)