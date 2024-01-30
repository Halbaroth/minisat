(lang dune 3.7)

(name minisat)
(generate_opam_files true)
(using menhir 2.1)
(using dune_site 0.1)

(source
 (github Halbaroth/minisat))

(authors "Pierre Villemot")

(maintainers "Pierre Villemot")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name minisat)
 (synopsis "An OCaml implementation of MiniSAT")
 (description "An OCaml implementation of MiniSAT")
 (depends
   ocaml
   dune
   dune-site
   cmdliner
   ppx_sexp_conv
   ppx_deriving
   fmt 
   logs
   base
   bheap
   stdio
   dimacs
   printbox
   printbox-text
   (ppx_inline_test :with-test)
   (ppx_assert :with-test))
 (tags
  (topics "to describe" your project))
 (sites
   (share satlib)))
