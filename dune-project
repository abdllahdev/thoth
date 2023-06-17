(lang dune 3.4)

(name thoth)

(generate_opam_files true)

(source
 (github algorizr/thoth))

(authors "Abdullah Elsayed <hi@algorizr.com>")

(maintainers "Abdullah Elsayed <hi@algorizr.com>")

(license MIT)

(documentation https://url/to/documentation)

(using menhir 2.1)

(package
 (name thoth)
 (synopsis "Multitier web language")
 (description
  "Multitier web language that compiles to ReactJS client and Node.js server in human-readable Typescript code")
 (depends ocaml dune core core_unix fmt fileutils jingoo cmdliner)
 (tags
  (dsl fullstack multitier web typescript "human-readable" reactjs "node.js")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
