(* Useful types for parsing *)

open Types

exception Parse_Error of string

type terminal = tok

type nonterminal = S | E | E' | F | F' | G | G' | H | H' | I

type symbol = Nt of nonterminal | Tm of terminal

type production = nonterminal * (symbol list)

(* possible actions to take when LL parsing *)
type action =
  | PREDICT of production
  | MATCH of tok
  | ACCEPT
  | REJECT

(* takes in top stack symbol and the next input token and selects action to take *)
type table = symbol -> tok -> action

(* abstract syntax tree *)
type ast =
  | Br of symbol * (ast ref) list
  | Lf of symbol

(* parsing state *)
type state = {
  stack : symbol list;
  ast_stack : ast ref list; (* list of ast refs corresponding to symbols on the stack *)
  input : tok list;
  finished : bool;
  accepted : bool
}
