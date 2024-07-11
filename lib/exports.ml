(* Functions we want to expose for usage within the api *)

(* Read string input and convert to proposition *)
let parse_input = Input_sanitization.Input_sanitizer.process

let find_satisfying_interpretation = Satisfiability.Satisfiable.satisfy


