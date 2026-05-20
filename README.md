# minuet

A small, single-threaded, effects-based cooperative scheduler for OCaml 5. Also 
includes a minimal HTTP/1.1 server and client proof of concept, showing the
scheduler can support real concurrent networking.

## Purpose

I built this as a learning project for OCaml 5's algebraic effects. The executor
is the focus, and the networking exists to show the scheduler is general
enough to power real networking code. The whole design is direct-style
cooperative scheduling: blocking calls look like ordinary blocking calls
but actually yield to other tasks.

## Features

- **Cooperative scheduler.** `spawn`, `join`, `yield`, `sleep`.
- **`spawn_blocking`.** Used to run truly blocking work on a fresh OS thread; 
  the result joins back from the executor.
- **IO reactor.** `select`-based fd readiness. `wait_readable` and
  `wait_writable` integrate with cooperative scheduling.
- **TCP socket primitives.** Non-blocking `listen`, `accept`, `connect`,
  `read`, `write`, `writev`, `close`, `shutdown`. They cooperatively wait
  when they need to.
- **HTTP/1.1 server and client.** A small cooperatively scheduled 
  proof of concept built on top of the scheduler using the
  [`h1`](https://github.com/robur-coop/ocaml-h1) state machine.
- **Direct-style API throughout.** `read`, `accept`, `request` block
  cooperatively and return their values, never futures or promises.
- **`ppx_expect` test suite.** Includes a stress test that drives 4000 HTTP
  requests through the scheduler with 100 always in flight, completing in
  less than 1.5 seconds.

## Cooperative blocking via effects

OCaml 5 added algebraic effects, allowing a function to suspend, hand its
continuation to a handler, and be resumed later. Other OCaml async
libraries like Lwt and Async predate this and use promises with chaining
instead. Eio is a newer full-featued IO library built directly on effects. 
Minuet is a much smaller demonstration of the same idea.

The entire scheduler (apart from `join` and `join_exn`) is built on one internal
effect: `Suspend`. Its handler takes the current task's continuation and hands 
it to a callback the caller provides, which controls when the task is resumed:

```ocaml
type _ Effect.t += Suspend : ((unit, unit) continuation -> unit) -> unit Effect.t
```

`yield` is "park me and immediately re-schedule." `sleep` is "park me on
a timer heap." `wait_readable` is "park me on this fd in the reactor's
table." Even bridging to callback-style libraries like `h1`'s
`yield_reader` and `yield_writer` is just one `Suspend` call. The entire
public scheduling API (`spawn`, `join`, `yield`, `sleep`, `wait_readable`,
`wait_writable`, `suspend`) is built on top of this one primitive.

Because suspension is a runtime mechanism and not a type-level one, the
same `Net.read fd buf` works from anywhere inside the top-level 
`Executor.start`. The caller never needs to know the task suspends.
This is how Minuet can be adapted easily to `h1`, a library designed for no 
particular runtime.

## Quick start

```bash
opam switch create . --deps-only  # create local switch + install deps (first time only)
eval $(opam env)                  # activate the switch in your current shell
dune build
dune exec minuet                  # the example: echoing HTTP server on :8132
curl -X POST -d 'hello' http://localhost:8132/   # -> "hello"
dune test                         # run the snapshot suite                        # run the snapshot suite
```

The example is in [`bin/example.ml`](bin/example.ml). The tests are in
[`test/test_minuet.ml`](test/test_minuet.ml).

## Project layout

```
lib/
  executor.ml/.mli   cooperative scheduler and reactor
  net.ml/.mli        TCP socket primitives over the scheduler
  http.ml/.mli       HTTP/1.1 server and client (h1 adapter)
  join_handle.ml     internal: spawn-result cell
bin/example.ml       echo server demo
test/test_minuet.ml  ppx_expect test suite
```
