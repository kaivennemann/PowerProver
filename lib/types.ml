(* Data types useful for our theorem prover *)

(* logical proposition *)
type proposition =
  | True
  | False
  | Lit of string
  | Not of proposition
  | And of proposition * proposition
  | Or of proposition * proposition
  | Implies of proposition * proposition
  | Iff of proposition * proposition

(* interpretation of propositional symbols *)
type interpretation = (string * bool) list

(* allows us to substitute any proposition for any atomic literal *)
type substitution = (string * proposition) list

(* tokens used by lexer and parser *)
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
