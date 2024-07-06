# PowerProver

## Overview

Experimentation with automated proofs.


## Useful Commands

- `dune build` to build the project
- `dune test` to run tests
- `dune exec power_prover` to execute the program


## Project Log

### Dune Setup

- `dune init proj power_prover` to create project directory (we rename the base dir as *PowerProver*)
- `dune build` to build the project
- `dune test` to run tests
- `dune exec power_prover` to execute the program

### Dream Setup

- `opam install dream`
- add *dream* to libraries in */bin/dune*: `(libraries power_prover dream)`
- add *dream* to package dependencies in */dune-project*: ` (depends ocaml dune dream)`


## Resources

- https://dune.readthedocs.io/en/latest/quick-start.html
- https://ocaml.org/docs/compiling-ocaml-projects
- https://aantron.github.io/dream/
