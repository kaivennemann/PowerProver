(* Sequent calculus: types and exceptions *)

(* exceptions that can occur when attempting a sequent proof *)
exception RuleNotApplicable
exception NotProvable of string
