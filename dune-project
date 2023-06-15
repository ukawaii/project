(lang dune 3.6)

(name project)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name project)
 (synopsis "A short synopsis")
 (depends ocaml dune ocamlgraph
  ppx_deriving
  ppx_deriving_hash
  ounit2
  qcheck-ounit
  ocaml-lsp-server
  utop))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
