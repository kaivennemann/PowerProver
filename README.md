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
- add Lwt preprocessing to */bin/dune*: `(preprocess (pps lwt_ppx))`
    - this tells dune to preprocess the code with an AST rewriter that can transform Lwt promises (e.g. `let%lwt = ...`)
- same with `ppx_yojson_conv` (Jane Street's library that converts between JSON and OCaml data types)

### Issues

- avoid giving a file the same name as its directory; Dune treats both files and directories as modules, so it can't distinguish between them if they have the same name


## To Do

### Short-Term

- lexer + parser for inputs
    - input sanitizer (no inputs longer than set number of vars)
- use OCaml modules
- get API up and running
- read into Lwt library

### Ideas

- brute force solver
- prolog solver
- sequent calculus solver (with intermediate steps)
    - tableau calculus
    - free variable tableau calculus
- propositional and first-order logic
- Hoare logic?

### Considerations

- none for now


## Resources

- https://dune.readthedocs.io/en/latest/quick-start.html
- https://ocaml.org/docs/compiling-ocaml-projects
- https://aantron.github.io/dream/
