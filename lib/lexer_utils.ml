(*
  Ordered lexer rules:

  true            => TRUE         [1,2,3,4]
  false           => FALSE        [5,6,7,8,9]
  [a-zA-Z]+ as s  => LIT s        [10]
  ¬               => NOT          [11]
  ∧               => AND          [12]
  ∨               => OR           [13]
  →               => IMPLIES      [14]
  ↔               => IFF          [15]
  (               => LEFT_PAREN   [16]
  )               => RIGHT_PAREN  [17]
  [ \t\n]         => SKIP         [18]
*)

(* Exceptions to aid control flow for lexer *)
exception Not_In_Alphabet of char
exception No_Transition of int * char

(* Types of tokens to be emitted *)
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

(* Tagged Deterministice Finite Automaton *)
type tdfa = {
  alphabet : char list;
  start_state: int;
  register_init : tok; (* initial contents of the single register for this TDFA *)
  delta : int * char -> int * (tok -> tok)
}

(* Description of a possible state that the TDFA could be in *)
type tdfa_state = {
  state_id : int;         (* current state *)
  register : tok;         (* contents of register *)
  tokens : tok list;      (* tokens emitted so far *)
  to_parse : char list;   (* characters left to parse *)
}

(* Function for stepping through a TDFA computation *)
let step (automaton : tdfa) (state : tdfa_state) : tdfa_state =
  match state.to_parse with
  | [] -> state
  | c :: cs ->
    try
      let next_state_id, f = automaton.delta (state.state_id, c) in
      let next_tok = f state.register in
      {
        state_id = next_state_id;
        register = next_tok;
        tokens = state.tokens;
        to_parse = cs
      }
    with (No_Transition (_, c)) ->
      let token_to_add = state.register in
      let next_state_id, f = automaton.delta (0, c) in
      let next_tok = f SKIP in
      {
        state_id = next_state_id;
        register = next_tok;
        tokens = token_to_add :: state.tokens;
        to_parse = cs
      }

(* Driver to perform a TDFA computation *)
let rec driver (automaton : tdfa) (state: tdfa_state) : tok list =
  if
    state.to_parse = []
  then
    List.rev (state.register :: state.tokens) (* ensure that final tag is emitted *)
  else
    driver automaton (step automaton state)
