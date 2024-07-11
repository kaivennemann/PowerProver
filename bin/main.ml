(* Entry point for this web application *)

(* open Power_prover.Types *)
open Power_prover.To_string
open Power_prover.Lib_utils
open Power_prover.Lexing.Lexer
open Power_prover.Parsing.Parser
open Power_prover.Satisfiability.Satisfiable

let () =
  let lex_result = lex "a & b & c = z > x & y | c" in
  let parse_result = parse lex_result in
  let p = simplify_true_false parse_result in
  let i = satisfy p in
  match i with
  | None -> ()
  | Some i -> print i string_of_interpr
  (* let p2 = interpret p ["b", false] in
  print_prop (to_nnf p2) *)

(* let () =
  let truth_vals = generate_truth_vals [Lit "a"; Lit "b"; Lit "c"; Lit "d"] in
  let _ = List.map print_interpretation (get 17 truth_vals) in () *)




let () = Api.Startapp.startapp ()
