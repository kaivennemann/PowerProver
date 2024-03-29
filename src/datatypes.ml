(* -------------------------------------------------------------------------------------------------------------------- *)
(* ----------------------------------- Data types useful for our theorem prover(s) ------------------------------------ *)
(* -------------------------------------------------------------------------------------------------------------------- *)

type proposition = (* logical proposition *)
  | True
  | False
  | Lit of string
  | Not of proposition
  | And of proposition * proposition
  | Or of proposition * proposition
  | Implies of proposition * proposition
  | Iff of proposition * proposition;;

type interpretation = (string * bool) list;; (* interpretation *)

type substitution = (string * proposition) list;; (* allows us to substitute any proposition for any atomic literal *)


(* -------------------------------------------------------------------------------------------------------------------- *)
(* ----------------------------------------- Utility functions for debugging ------------------------------------------ *)
(* -------------------------------------------------------------------------------------------------------------------- *)

let rec string_of_prop = function
  | True -> "True"
  | False -> "False"
  | Lit symbol -> symbol
  | Not p -> "¬" ^ (string_of_prop p)
  | And (p1, p2) -> "(" ^ (string_of_prop p1) ^ " ∧ " ^ (string_of_prop p2) ^ ")"
  | Or (p1, p2) -> "(" ^ (string_of_prop p1) ^ " ∨ " ^ (string_of_prop p2) ^ ")"
  | Implies (p1, p2) -> "(" ^ (string_of_prop p1) ^ " → " ^ (string_of_prop p2) ^ ")"
  | Iff (p1, p2) -> "(" ^ (string_of_prop p1) ^ " ↔ " ^ (string_of_prop p2) ^ ")";;

  let string_of_sub sub =
    let rec stringify = function
      | [] -> ""
      | (s, value) :: [] -> "(" ^ s ^ ": " ^ (string_of_prop value) ^ ")"
      | (s, value) :: tl -> "(" ^ s ^ ": " ^ (string_of_prop value) ^ "), " ^ (stringify tl)
    in "[" ^ (stringify sub) ^ "]";;

let string_of_interpr (i : interpretation) =
  let rec stringify = function
    | [] -> ""
    | (s, value) :: [] -> "(" ^ s ^ ": " ^ (string_of_bool value) ^ ")"
    | (s, value) :: tl -> "(" ^ s ^ ": " ^ (string_of_bool value) ^ "), " ^ (stringify tl)
  in "[" ^ (stringify i) ^ "]";;

let print_prop prop = print_endline (string_of_prop prop);;


(* -------------------------------------------------------------------------------------------------------------------- *)
(* ------------------------------ Utility functions for dealing with our new data types ------------------------------- *)
(* -------------------------------------------------------------------------------------------------------------------- *)

(* Converts an interpretation to a substitution *)
let rec sub_of_interpr = function
  | [] -> []
  | (s, b) :: tl -> (s, if b then True else False) :: (sub_of_interpr tl);;

(* Removes instances of True or False literals via simplification *)
let rec simplify_true_false = function
  | True -> True
  | False -> False
  | Lit symbol -> Lit symbol
  | Not p ->
    (match simplify_true_false p with
    | True -> False
    | False -> True
    | Not p' -> p'
    | p' -> Not(p'))
  | And (p1, p2) ->
    (match simplify_true_false p1, simplify_true_false p2 with
    | False, _ -> False
    | _, False -> False
    | True, p2' -> p2'
    | p1', True -> p1'
    | p1', p2' -> And(p1', p2'))
  | Or (p1, p2) ->
    (match simplify_true_false p1, simplify_true_false p2 with
    | True, _ -> True
    | _, True -> True
    | False, p2' -> p2'
    | p1', False -> p1'
    | p1', p2' -> Or(p1', p2'))
  | Implies (p1, p2) -> 
    (match simplify_true_false p1, simplify_true_false p2 with
    | False, _ -> True
    | True, p2' -> p2'
    | p1', p2' -> Implies(p1', p2'))
  | Iff (p1, p2) -> 
    (match simplify_true_false p1, simplify_true_false p2 with
    | True, p2' -> p2'
    | p1', True -> p1'
    | False, p2' -> simplify_true_false (Not(p2'))
    | p1', False -> simplify_true_false (Not(p1'))
    | p1', p2' -> Iff(p1', p2'));;

(* Finds a symbol in a list of substitutions and returns the value to substitute for that symbol *)
let rec replace sub symbol =
  match sub with
  | [] -> Lit symbol
  | (s, value) :: sub -> if s = symbol then value else replace sub symbol;;

(* Substitutes propositions for symbols as specified by the sub argument *)
let rec substitute (prop : proposition) (sub : substitution) =
  match prop with
  | True -> True
  | False -> False
  | Lit s -> replace sub s
  | Not p -> Not (substitute p sub)
  | And(p1, p2) -> And(substitute p1 sub, substitute p2 sub)
  | Or(p1, p2) -> Or(substitute p1 sub, substitute p2 sub)
  | Implies(p1, p2) -> Implies(substitute p1 sub, substitute p2 sub)
  | Iff(p1, p2) -> And(substitute p1 sub, substitute p2 sub);;

(* Converts a proposition to negation normal form *)
let to_nnf prop =
  let rec to_nnf = function
    | True -> True
    | False -> False
    | Lit symbol -> Lit symbol
    | Not p ->
      (match to_nnf p with
      | And(p1, p2) -> Or(to_nnf (Not(p1)), to_nnf (Not(p2)))
      | Or(p1, p2) -> And(to_nnf (Not(p1)), to_nnf (Not(p2)))
      | p' -> simplify_true_false (Not p'))
    | And(p1, p2) -> And(to_nnf p1, to_nnf p2)
    | Or(p1, p2) -> Or(to_nnf p1, to_nnf p2)
    | Implies(p1, p2) -> to_nnf (Or(Not(p1), p2))
    | Iff(p1, p2) -> to_nnf (And(Implies(p1, p2), Implies(p2, p1)))
  in
  simplify_true_false (to_nnf prop);;

(* Applies an interpretation to a proposition and returns the resulting proposition *)
let interpret prop i = simplify_true_false (substitute prop (sub_of_interpr i));;

(* Tests whether a proposition holds under a given interpretation *)
let test_interpretation prop i =
  match interpret prop i with
  | True -> Some true
  | False -> Some false
  | _ -> None;; (* return None if cannot be determined *)


(* -------------------------------------------------------------------------------------------------------------------- *)
(* ------------------------------------------------------ Examples ---------------------------------------------------- *)
(* -------------------------------------------------------------------------------------------------------------------- *)

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

show_example p1 i1;;
show_example p1 i2;;
show_example p1 i3;;

show_example p2 i1;;
show_example p2 i2;;
show_example p2 i3;;
