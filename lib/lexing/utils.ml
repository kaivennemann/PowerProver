open Types
open Headers

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
        tokens = if token_to_add <> SKIP then token_to_add :: state.tokens else state.tokens;
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
