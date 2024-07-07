open Power_prover.Datatypes
open Power_prover.Utils
open Power_prover.To_string
open Power_prover.Lexer

let show_example p i =
  print_endline ("Proposition p:     " ^ (string_of_prop p));
  print_endline ("p in NNF:          " ^ (string_of_prop (to_nnf p)));
  print_endline ("Interpretation i:  " ^ (string_of_interpr i));
  print_endline ("p under i:         " ^ (string_of_prop (interpret p i)) ^ "\n");;

let p1 = Not(Not(Not(And(Lit "a", Lit "b"))));;
let p2 = Iff(Not(Implies(Or(Lit "a", Lit "b"), And(Lit "b", Lit "c"))), True);;
let i1 = [("a", false); ("b", true); ("c", false)];;
let i2 = [("a", false); ("b", false)];;
let i3 = [("c", true)];;

let () =
  show_example p1 i1;
  show_example p1 i2;
  show_example p1 i3;

  show_example p2 i1;
  show_example p2 i2;
  show_example p2 i3;;

let _ = 
  let tokens = lex "a|b&&     i love sushi" in
  let s = string_of_tokens tokens in
  print_endline s
(* TODO: fix LIT issue *)