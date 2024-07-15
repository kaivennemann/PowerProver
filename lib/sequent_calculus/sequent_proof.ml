(* Sequent Calculus *)

open Types
open Headers
open Sequent_rules

(* returns true if arg is the basic tautological sequent *)
let rec is_basic_sequent (Seq(l1, l2)) =
  match l1 with
  | [] -> false
  | p :: ps -> List.mem p l2 || is_basic_sequent (Seq (ps, l2))

(* gives list of sequents that need to be proved for this to be true *)
let get_dependencies (s : seq) : seq list =
  let rec apply_each fns s = (* apply each fn to s and return first result *)
    match fns with
    | [] -> raise (NotProvable "Couldn't find proof")
    | fn :: fns ->
      try
        fn s
      with
      | RuleNotApplicable -> apply_each fns s
  in
  if is_basic_sequent s then
    []
  else
    apply_each rule_precedence_list s

(* generates a proof for a sequent in stree form or throws an exception *)
(* TODO: deal with <-> case *)
let rec prove_sequent (s : seq) : stree =
  let dependencies = get_dependencies s in
  let proofs = List.map prove_sequent dependencies in
  Concl(s, proofs)
