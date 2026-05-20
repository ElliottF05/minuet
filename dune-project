(lang dune 3.23)
(name minuet)

(generate_opam_files true)

(package
 (name minuet)
 (synopsis
  "A small, single-threaded, effects-based cooperative scheduler for OCaml 5")
 (depends
  (ocaml (>= 5.0.0))
  dune
  core
  core_unix
  bigstring-unix
  h1
  (ppx_jane :with-test)))
