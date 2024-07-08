(* Types for lexing *)

open Types

(* Exceptions to aid control flow for lexer *)
exception Not_In_Alphabet of char
exception No_Transition of int * char

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
