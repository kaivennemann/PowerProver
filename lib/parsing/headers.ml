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

(* mutable abstract syntax tree *)
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

(* convert ast to proposition *)
let rec prop_of_ast (tree : ast) : proposition =
  match tree with
  | Lf (Tm TRUE) -> True
  | Lf (Tm FALSE) -> False
  | Lf (Tm (LIT s)) -> Lit s
  | Br (Nt S, e_ref :: _) -> prop_of_ast !e_ref
  | Br (Nt I, _ :: e_ref :: _ :: _) -> prop_of_ast !e_ref (* I ::= LEFT_PAREN E RIGHT_PAREN *)
  | Br (Nt I, _ :: i_ref :: _) -> let inner_prop = prop_of_ast !i_ref in Not(inner_prop) (* I ::=  NOT I *)
  | Br (Nt I, [some_ref]) -> (* I ::= TRUE | FALSE | LIT s *)
    begin
      match !some_ref with
      | Lf (Tm TRUE) -> True
      | Lf (Tm FALSE) -> False
      | Lf (Tm (LIT s)) -> Lit s
      | _ -> failwith "some bug"
    end
  | Br (Nt a, b_ref :: a'_ref :: _) -> (* A ::= B A' *)
    begin
      match !a'_ref with
      | Lf _ -> prop_of_ast !b_ref (* A' ::= eps *)
      | Br (_, _ :: tl) -> (* A' ::=  CONNECTOR B A' *)
        let left = prop_of_ast !b_ref in
        let right = prop_of_ast (Br(Nt a, tl)) in
        begin
          match a with
          | E -> Iff(left, right)
          | F -> Implies(left, right)
          | G -> Or(left, right)
          | H -> And(left, right)
          | _ -> failwith "a bug!"
        end
      | _ -> failwith "another bug"
    end
  | _ -> failwith "found a bug!"
  