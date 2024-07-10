open Types
open Headers
open Utils

let my_productions : production list = (* currently not in use *)
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


let my_action_table (sym : symbol) (tk : tok) : action =
  let predict_if_in_first tk prod =
    match tk with
    | TRUE -> PREDICT prod
    | FALSE -> PREDICT prod
    | LIT _ -> PREDICT prod
    | NOT -> PREDICT prod
    | LEFT_PAREN -> PREDICT prod
    | _ -> REJECT
  in

  let predict_if_in_follow tk follow prod =
    if
      List.mem tk follow
    then
      PREDICT prod
    else
      REJECT
  in

  match sym, tk with
  | Tm (LIT "$"), LIT "$" -> ACCEPT
  | Tm t, tk -> if t = tk then MATCH t else REJECT
  | Nt S, tk -> predict_if_in_first tk (S, [Nt E; Tm (LIT "$")])
  | Nt E, tk -> predict_if_in_first tk (E, [Nt F; Nt E'])
  | Nt E', IFF -> PREDICT (E', [Tm IFF; Nt F; Nt E'])
  | Nt E', tk -> predict_if_in_follow tk [LIT "$"; RIGHT_PAREN] (E', [])
  | Nt F, tk -> predict_if_in_first tk (F, [Nt G; Nt F'])
  | Nt F', IMPLIES -> PREDICT (F', [Tm IMPLIES; Nt G; Nt F'])
  | Nt F', tk -> predict_if_in_follow tk [IFF; LIT "$"; RIGHT_PAREN] (F', [])
  | Nt G, tk -> predict_if_in_first tk (G, [Nt H; Nt G'])
  | Nt G', OR -> PREDICT (G', [Tm OR; Nt H; Nt G'])
  | Nt G', tk -> predict_if_in_follow tk [IMPLIES; IFF; LIT "$"; RIGHT_PAREN] (G', [])
  | Nt H, tk -> predict_if_in_first tk (H, [Nt I; Nt H'])
  | Nt H', AND -> PREDICT (H', [Tm AND; Nt I; Nt H'])
  | Nt H', tk -> predict_if_in_follow tk [OR; IMPLIES; IFF; LIT "$"; RIGHT_PAREN] (H', [])
  | Nt I, NOT -> PREDICT (I, [Tm NOT; Nt I]);
  | Nt I, LEFT_PAREN -> PREDICT (I, [Tm LEFT_PAREN; Nt E; Tm RIGHT_PAREN]);
  | Nt I, TRUE -> PREDICT (I, [Tm TRUE]);
  | Nt I, FALSE -> PREDICT (I, [Tm FALSE]);
  | Nt I, LIT s when s <> "$" -> PREDICT (I, [Tm (LIT s)])
  | _ -> REJECT


let parse (input : tok list) =
  let root = ref (Lf (Nt S)) in
  let initial_parse_state = {
    stack = [Nt S];
    ast_stack = [root]; (* list of ast refs corresponding to symbols on the stack *)
    input = input @ [LIT "$"];
    finished = false;
    accepted = false
  } in
  driver initial_parse_state my_action_table;
  print_endline (string_of_ast (!root))
