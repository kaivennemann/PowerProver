(* Data types useful for our theorem prover(s) *)

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


(* Utility functions to deal with the data types *)

let rec string_of_prop = function
  | True -> "True"
  | False -> "False"
  | Lit symbol -> symbol
  | Not p -> "¬" ^ (string_of_prop p)
  | And (p1, p2) -> "(" ^ (string_of_prop p1) ^ " ∧ " ^ (string_of_prop p2) ^ ")"
  | Or (p1, p2) -> "(" ^ (string_of_prop p1) ^ " ∨ " ^ (string_of_prop p2) ^ ")"
  | Implies (p1, p2) -> "(" ^ (string_of_prop p1) ^ " → " ^ (string_of_prop p2) ^ ")"
  | Iff (p1, p2) -> "(" ^ (string_of_prop p1) ^ " ↔ " ^ (string_of_prop p2) ^ ")";;

let string_of_interpr (i : interpretation) =
  let rec stringify = function
    | [] -> ""
    | (s, value) :: [] -> "(" ^ s ^ ": " ^ (string_of_bool value) ^ ")"
    | (s, value) :: tl -> "(" ^ s ^ ": " ^ (string_of_bool value) ^ "), " ^ (stringify tl)
  in "[" ^ (stringify i) ^ "]";;

let print_prop prop = print_endline (string_of_prop prop)

let rec find (i : interpretation) symbol =
  match i with
  | [] -> None
  | (s, value) :: i ->
    if s = symbol then
      (if value then Some True else Some False)
    else
      find i symbol;;

(* Apply all relevant mappings from an interpretation to the symbols of a proposition *)
let rec interpret prop i =
  match prop with
  | True -> prop
  | False -> prop
  | Lit symbol -> 
    (match find i symbol with
    | None -> prop
    | Some a -> a)
  | Not prop' ->
    (match interpret prop' i with
    | True -> False
    | False -> True
    | _ -> prop)
  | And (p1, p2) ->
    (match interpret p1 i, interpret p2 i with
    | False, _ -> False
    | _, False -> False
    | True, p2' -> p2'
    | p1', True -> p1'
    | _ -> prop)
  | Or (p1, p2) ->
    (match interpret p1 i, interpret p2 i with
    | True, _ -> True
    | _, True -> True
    | False, p2' -> p2'
    | p1', False -> p1'
    | _ -> prop)
  | Implies (p1, p2) -> interpret (Or(Not(p1), p2)) i
  | Iff (p1, p2) -> interpret (And(Implies(p1, p2), Implies(p2, p1))) i;;

(* Tests whether a proposition holds under a given interpretation *)
let test prop i =
  match interpret prop i with
  | True -> Some true
  | False -> Some false
  | _ -> None;; (* return None if cannot be determined *)

(* Convert a proposition to negation normal form *)
let to_nnf prop = "TODO"


(* Examples *)
let show_example p i =
  print_endline ("Proposition p:     " ^ (string_of_prop p));
  print_endline ("Interpretation i:  " ^ (string_of_interpr i));
  print_endline ("p under i:         " ^ (string_of_prop (interpret p i)) ^ "\n");;

let p1 = Iff(Not(Implies(Or(Lit "a", Lit "b"), And(Lit "b", Lit "c"))), True);;
let i1 = [("a", false); ("b", true); ("c", false)];;
let i2 = [("a", false); ("b", false)];;
let i3 = [("c", true)];;

show_example p1 i1;;
show_example p1 i2;;
show_example p1 i3;;
(* print_endline (match test p i with None -> "None" | Some a -> string_of_bool a);; *)
