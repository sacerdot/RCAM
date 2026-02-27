(lang dune 3.21)

(name rcam)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Nicolò Pizzo <nicolo.pizzo2@unibo.it>" "Claudio Sacerdoti Coen <claudio.sacerdoticoen@unibo.it>")

(maintainers "Claudio Sacerdoti Coen Name <claudio.sacerdoticoen@unibo.it>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name rcam)
 (synopsis "Reversible Crumbling Abstract Machine")
 (description "The implementation in OCaml of the Reversible Crubling Abstract Machine")
 (depends ocaml)
 (tags
  ("plotkin cbv" "reversible computation" "abstract machine" "crumbling")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
