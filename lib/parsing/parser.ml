open Types
open Headers
(* open Utils *)

let my_productions : production list =
  [
    (S, [Nt E; Tm (LIT "$")]);    (* S ::= E $ *)
    (E, [Nt F; Nt E']);
    (E', [Tm IFF; Nt F; Nt E']);
    (E', []);
    (F, [Nt G; Nt F']);
    (F', [Tm IMPLIES; Nt G; Nt F']);
    (F', []);
    (G, [Nt H; Nt G']);
    (G', [Tm OR; Nt H; Nt G']);
    (G', []);
    (H, [Nt I; Nt H']);
    (H', [Tm AND; Nt I; Nt H']);
    (H', []);
    (I, [Tm NOT; Nt I]);
    (I, [Tm LEFT_PAREN; Nt E; Tm RIGHT_PAREN]);
    (I, [Tm TRUE]);
    (I, [Tm FALSE]);
    (I, [Tm (LIT "any string")])
  ]


(* let my_action_table (sym : symbol) (tk : tok) : action =
  match sym, tk with
  | Tm t, tk -> if t = tk then MATCH t else REJECT
  | Nt S, tk -> if List.mem tk [] *)


(* let parse (input : tok list) : proposition =
  let root = ref (Lf (Nt S)) in
  let initial_parse_state = {
    stack = [Nt S];
    ast_stack = [root]; (* list of ast refs corresponding to symbols on the stack *)
    input = input;
    finished = false;
    accepted = false
  } in
  driver initial_parse_state my_action_table;
  proposition_of_ast (!root) *)
