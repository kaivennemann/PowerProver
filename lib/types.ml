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
type interpretation = (string * bool) list (* note: we're using OCaml booleans here *)

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

(* sequent *)
type seq = Seq of proposition list * proposition list 

(* tree of sequents where Conclusion(s, l) represents a sequent s and a list l of
 * strees that together prove s *)
type stree = Concl of seq * stree list
