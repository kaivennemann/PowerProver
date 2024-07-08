open Types

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

let string_of_tok = function
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | LIT s -> "LIT " ^ s
  | NOT -> "NOT"
  | AND -> "AND"
  | OR -> "OR"
  | IMPLIES -> "IMPLIES"
  | IFF -> "IFF"
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | SKIP -> "SKIP"

let rec string_of_tokens = function
  | [] -> ""
  | t :: ts -> "[" ^ string_of_tok t ^ "] " ^ string_of_tokens ts
