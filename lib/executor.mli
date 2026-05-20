type 'a join_handle

val spawn : (unit -> 'a) -> 'a join_handle

val spawn_blocking : (unit -> 'a) -> 'a join_handle

val join : 'a join_handle -> ('a, exn) Result.t
val join_exn : 'a join_handle -> 'a

val yield : unit -> unit
val sleep : float -> unit
val wait_readable : Core_unix.File_descr.t -> unit
val wait_writable : Core_unix.File_descr.t -> unit

val suspend : ((unit -> unit) -> unit) -> unit

val start : (unit -> unit) -> unit