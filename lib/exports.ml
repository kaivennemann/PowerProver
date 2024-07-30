(* Functions we want to expose for usage within the api *)

(* Read string input and convert to proposition *)
let parse_input = Input_sanitization.Input_sanitizer.process

(* Find a truth assignment satisfying the input proposition or return None *)
let find_satisfying_interpretation = Satisfiability.Satisfiable.satisfy

(* Find a sequent calculus proof or return None if proposition not provable *)
let get_sequent_calculus_proof = Sequent_calculus.Sequent_proof.prove

let get_tableau_calculus_proof = () (* TODO *)
