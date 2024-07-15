# PowerProver

## Overview

An automated theorem-prover for propositional logic. This will be updated for first-order logic. The prover is exposed as an API using the Dream web framework.

Steps:
1. Lex the raw input (a string representing the formula to prove) to generate a list of tokens.
2. Parse the token list to generate a syntax tree.
3. Check validity? (expensive operation)
4. Run sequent calculus computation on the negation of the formula.
5. Return list of sequents that prove the formula.

## Lexing

We first lex the input, obtaining the following possible tokens:

```
type tok =
  | TRUE
  | FALSE
  | LIT of string
  | NOT
  | AND
  | OR
  | IMPLIES
  | IFF
  | LEFT_PAREN
  | RIGHT_PAREN
  | SKIP
```

## Parsing

First, we decided on an appropriate grammar for parsing the token list.

### Approach 1: Naive Grammar

#### Simple CFG

```
S ::= E $
E ::= TRUE
  | FALSE
  | LIT s
  | NOT E
  | E AND E
  | E OR E
  | E IMPLIES E
  | E IFF E
  | LEFT_PAREN E RIGHT_PAREN
```

#### Issues
- we need to encode precedence
- above grammar is ambiguous
    - consider `NOT E OR E`, which can be parenthesized as `(NOT E) OR E` vs. `NOT (E OR E)`

### Approach 2: LL(1) Grammar

#### CFG redefined as LL(1) grammar

```
S  ::=  E $
E  ::=  F E'
E' ::=  IFF F E' | eps
F  ::=  G F'
F' ::=  IMPLIES G F' | eps
G  ::=  H G'
G' ::=  OR H G' | eps
H  ::=  I H'
H' ::=  AND I H' | eps
I  ::=  NOT I | LEFT_PAREN E RIGHT_PAREN | TRUE | FALSE | LIT s
```

#### FIRST Sets

```
FIRST(S)  = FIRST(E)
          = FIRST(F)
          = FIRST(G)
          = FIRST(H)
          = FIRST(I)
          = {TRUE, FALSE, LIT s, NOT, LEFT_PAREN}
FIRST(E') = {IFF, eps}
FIRST(F') = {IMPLIES, eps}
FIRST(G') = {OR, eps}
FIRST(H') = {AND, eps}
```

#### FOLLOW Sets

```
FOLLOW(E)                                           = {$, RIGHT_PAREN}
FOLLOW(E')  = FOLLOW(E)                             = {$, RIGHT_PAREN}
FOLLOW(F)   = FIRST(E') U FOLLOW(E') U FOLLOW(E)    = {IFF, $, RIGHT_PAREN}
FOLLOW(F')  = FOLLOW(F)                             = {IFF, $, RIGHT_PAREN}
FOLLOW(G)   = FIRST(F') U FOLLOW(F') U FOLLOW(F)    = {IMPLIES, IFF, $, RIGHT_PAREN}
FOLLOW(G')  = FOLLOW(G)                             = {IMPLIES, IFF, $, RIGHT_PAREN}
FOLLOW(H)   = FIRST(G') U FOLLOW(G') U FOLLOW(G)    = {OR, IMPLIES, IFF, $, RIGHT_PAREN}
FOLLOW(H')  = FOLLOW(H)                             = {OR, IMPLIES, IFF, $, RIGHT_PAREN}
FOLLOW(I)   = FIRST(H') U FOLLOW(H') U FOLLOW(H)    = {AND, OR, IMPLIES, IFF, $, RIGHT_PAREN}
```

#### Notes

- this approach can work, but the grammar is quite convoluted
- LL parsers are relatively straightforward to implement, however, so we opt for this approach (for now)

### Approach 3: LR Grammar

#### LR(1) Grammar

```
S  ::=  E $
E  ::=  E IFF F | F
F  ::=  F IMPLIES G | G
G  ::=  G OR H | H
H  ::=  H AND I | I
I  ::=  NOT I | LEFT_PAREN E RIGHT_PAREN | TRUE | FALSE | LIT s 
```

#### FIRST Sets

```
FIRST(S)  = FIRST(E)
          = FIRST(F)
          = FIRST(G)
          = FIRST(H)
          = FIRST(I)
          = {TRUE, FALSE, LIT s, NOT, LEFT_PAREN}
```

#### FOLLOW Sets

```
TODO
```

#### Notes

- this is probably the best approach with the simplest grammar
- we might switch to an LR parser in the future


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

- input sanitizer (no inputs longer than set number of vars, no $ signs)
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
- other L&P parts?
- Hoare logic?

### Fixes

- none for now 


## Resources

- https://dune.readthedocs.io/en/latest/quick-start.html
- https://ocaml.org/docs/compiling-ocaml-projects
- https://aantron.github.io/dream/
