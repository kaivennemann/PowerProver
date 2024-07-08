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

(*

Simple CFG:

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

CFG, redefined to include precedence:

S  ::=  E $
E  ::=  F E'
E' ::=  IFF F E' | eps
F  ::=  G F'
F' ::=  IMPLIES G F' | eps
G  ::=  H G'
G' ::=  OR H G' | eps
H  ::=  I H'
H' ::=  AND I H' | eps
I  ::=  NOT I | J
J  ::=  LEFT_PAREN E RIGHT_PAREN | TRUE | FALSE | LIT s

FIRST Sets:

FIRST(S)  = FIRST(E)
          = FIRST(F)
          = FIRST(G)
          = FIRST(H)
          = FIRST(I)
          = {TRUE, FALSE, LIT s, NOT, LEFT_PAREN}
FIRST(J)  = {TRUE, FALSE, LIT s, LEFT_PAREN}
FIRST(E') = {IFF, eps}
FIRST(F') = {IMPLIES, eps}
FIRST(G') = {OR, eps}
FIRST(H') = {AND, eps}

FOLLOW Sets:

FOLLOW(E)                                           = {$, RIGHT_PAREN}
FOLLOW(J)   = FOLLOW(I)                             = {AND, OR, IMPLIES, IFF, $, RIGHT_PAREN}
FOLLOW(I)   = FIRST(H') U FOLLOW(H') U FOLLOW(H)    = {AND, OR, IMPLIES, IFF, $, RIGHT_PAREN}
FOLLOW(H')  = FOLLOW(H)                             = {OR, IMPLIES, IFF, $, RIGHT_PAREN}
FOLLOW(H)   = FIRST(G') U FOLLOW(G') U FOLLOW(G)    = {OR, IMPLIES, IFF, $, RIGHT_PAREN}
FOLLOW(G')  = FOLLOW(G)                             = {IMPLIES, IFF, $, RIGHT_PAREN}
FOLLOW(G)   = FIRST(F') U FOLLOW(F') U FOLLOW(F)    = {IMPLIES, IFF, $, RIGHT_PAREN}
FOLLOW(F')  = FOLLOW(F)                             = {IFF, $, RIGHT_PAREN}
FOLLOW(F)   = FIRST(E') U FOLLOW(E') U FOLLOW(E)    = {IFF, $, RIGHT_PAREN}
FOLLOW(E')  = FOLLOW(E)                             = {$, RIGHT_PAREN}

*)
