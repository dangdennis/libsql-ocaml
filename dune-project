(lang dune 3.12)

(name libsql)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name libsql)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  ocamlformat
  (ctypes
   (= 0.21.1))
  (ctypes-foreign
   (= 0.21.1)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
