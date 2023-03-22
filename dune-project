(lang dune 3.4)

(name ralang)

(generate_opam_files true)

(source
 (github onlyabdullah/ralang))

(authors "Abdullah Elsayed")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 2.1)

(package
 (name ralang)
 (synopsis "Multitier web language")
 (description
  "Multitier web language that compiles to human-readable code in ExpressJS and ReactJS")
 (depends ocaml dune core core_unix fmt str fileutils jingoo cmdliner)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
