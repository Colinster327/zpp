(lang dune 3.16)

(using menhir 2.1)

(name zpp)

(generate_opam_files true)

(authors "Colin Murphy <Colinster327>")

(maintainers "Colin Murphy <Colinster327>")

(license MIT)

(package
 (name zpp)
 (synopsis "gen z programming language")
 (description "A programming language for people of generation Z")
 (depends ocaml dune ounit menhir)
 (tags
  (ocaml interpreter gen-z)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
