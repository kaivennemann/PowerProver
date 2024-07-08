(* Lexer for transforming an input string into a list of tokens *)

open Types
open Headers
open Utils

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

(* Define a TDFA *)
let my_tdfa : tdfa =
  let my_alphabet =
    let lowercase = List.init 26 (fun i -> Char.chr (Char.code 'a' + i)) in
    let uppercase = List.init 26 (fun i -> Char.chr (Char.code 'A' + i)) in
    ['!'; '&'; '|'; '>'; '='; '('; ')'; ' '; '\t'; '\n'] @ lowercase @ uppercase
  in

  let my_delta (i, c) =
    if
      not (List.mem c my_alphabet)
    then
      raise (Not_In_Alphabet c)
    else
      let append_lit = function
          | LIT s -> LIT (s ^ Char.escaped c)
          | _ -> LIT (Char.escaped c)
      in

      let is_ltr c =
        let ascii = Char.code c in
        (65 <= ascii && ascii <= 90) || (97 <= ascii && ascii <= 122)
      in

      match i, c with
      | 0, '!'                  -> 11, (fun _ -> NOT)
      | 0, '&'                  -> 12, (fun _ -> AND)
      | 0, '|'                  -> 13, (fun _ -> OR)
      | 0, '>'                  -> 14, (fun _ -> IMPLIES)
      | 0, '='                  -> 15, (fun _ -> IFF)
      | 0, '('                  -> 16, (fun _ -> LEFT_PAREN)
      | 0, ')'                  -> 17, (fun _ -> RIGHT_PAREN)
      | 0, ' '                  -> 18, (fun _ -> SKIP)
      | 0, '\t'                 -> 18, (fun _ -> SKIP)
      | 0, '\n'                 -> 18, (fun _ -> SKIP)
      | 0, 't'                  -> 1, append_lit
      | 0, 'f'                  -> 5, append_lit
      | 0, c when is_ltr c      -> 10, append_lit
      | 1, 'r'                  -> 2, append_lit
      | 1, c when is_ltr c      -> 10, append_lit
      | 2, 'u'                  -> 3, append_lit
      | 2, c when is_ltr c      -> 10, append_lit
      | 3, 'e'                  -> 4, (fun _ -> TRUE)
      | 3, c when is_ltr c      -> 10, append_lit
      | 4, c when is_ltr c      -> 10, (fun _ -> (LIT ("true" ^ Char.escaped c)))
      | 5, 'a'                  -> 6, append_lit
      | 5, c when is_ltr c      -> 10, append_lit
      | 6, 'l'                  -> 7, append_lit
      | 6, c when is_ltr c      -> 10, append_lit
      | 7, 's'                  -> 8, append_lit
      | 7, c when is_ltr c      -> 10, append_lit
      | 8, 'e'                  -> 9, (fun _ -> FALSE)
      | 8, c when is_ltr c      -> 10, append_lit
      | 9, c when is_ltr c      -> 10, (fun _ -> (LIT ("false" ^ Char.escaped c)))
      | 10, c when is_ltr c     -> 10, append_lit
      | _                       -> raise (No_Transition (i, c))
  in

  {
    alphabet = my_alphabet;
    start_state = 0;
    register_init = SKIP;
    delta = my_delta
  }


let lex (s : string) : tok list =
  let to_parse = s |> String.to_seq |> List.of_seq in
  let initial_tdfa_state = {
    state_id = my_tdfa.start_state;
    register = my_tdfa.register_init;
    tokens = [];
    to_parse = to_parse
  } in
  driver my_tdfa initial_tdfa_state
