(* Lexer for transforming an input string into a list of tokens *)

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

(*
Ordered lexer rules:



Example inputs:


*)
