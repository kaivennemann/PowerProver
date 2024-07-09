open Headers

(* step through parse states *)
let step (parse_state : state) (action_table : table) =
  match parse_state.stack, parse_state.ast_stack, parse_state.input with
  | x::xs, t::ts, i::is -> (
      match action_table x i with
      | ACCEPT -> {stack = xs; ast_stack = ts; input = is; finished = true; accepted = true}
      | REJECT -> {stack = xs; ast_stack = ts; input = is; finished = true; accepted = false}
      | MATCH _ -> {stack = xs; ast_stack = ts; input = is; finished = false; accepted = false}
      | PREDICT (_, l) ->
        let update_ast_ref sym = (* for each symbol in the production, create an ast ref *)
          let new_lf = ref (Lf sym) in
          (match !t with
          | Lf t_sym -> t := Br (t_sym, [new_lf])
          | Br (t_sym, l) -> t := Br(t_sym, l @ [new_lf]));
          new_lf
        in
        {
          stack = l @ xs; (* add production symbols to symbol stack *)
          ast_stack = List.map update_ast_ref l @ ts; (* update t's contents and add new refs to ast_stack *)
          input = i :: is;
          finished = false;
          accepted = false
        }
    )
  | xs, ts, is -> {stack = xs; ast_stack = ts; input = is; finished = true; accepted = false}

(* driver function to perform multistepping *)
let rec driver (parse_state : state) (action_table : table) : unit =
  if
    parse_state.finished
  then
    if
      parse_state.accepted
    then
      ()
    else
      raise (Parse_Error "Input rejected")
  else
    driver (step parse_state action_table) action_table
  