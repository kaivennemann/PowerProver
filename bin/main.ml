(* Entry point for this web application *)

(* open Power_prover.Types *)
open Power_prover.To_string
(* open Power_prover.Lib_utils *)
open Power_prover.Lexing.Lexer
open Power_prover.Parsing.Parser
(* open Power_prover.Satisfiability.Satisfiable *)

let () =
  let lex_result = lex "(p > (q > (p > p)))" in (* "a & b & c = z > x & y | c" *)
  let parse_result = parse lex_result in
  let proof = Power_prover.Sequent_calculus.Sequent_proof.prove_sequent (Seq([], [parse_result])) in
  let Br(s, _) = proof in
  print string_of_seq s

let () = Api.Startapp.startapp ()
