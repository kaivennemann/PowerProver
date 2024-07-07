(* Data types useful for our theorem prover *)

type proposition = (* logical proposition *)
  | True
  | False
  | Lit of string
  | Not of proposition
  | And of proposition * proposition
  | Or of proposition * proposition
  | Implies of proposition * proposition
  | Iff of proposition * proposition;;

type interpretation = (string * bool) list;; (* interpretation *)

type substitution = (string * proposition) list;; (* allows us to substitute any proposition for any atomic literal *)

