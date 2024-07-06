(lang dune 3.13)

(name power_prover)

(generate_opam_files true)

(source
 (github kaivennemann/PowerProver))

(authors "Kai Vennemann")

(license MIT)

(package
 (name power_prover)
 (synopsis "Experimentation with automated proofs")
 (depends ocaml dune dream ppx_yojson_conv))
