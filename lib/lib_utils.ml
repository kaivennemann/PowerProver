(* Utility functions *)

open Types

(* Converts an interpretation to a substitution *)
let rec sub_of_interpr = function
  | [] -> []
  | (s, b) :: tl -> (s, if b then True else False) :: (sub_of_interpr tl)

(* Removes instances of True or False literals via simplification as well as repeated Nots *)
let rec simplify_true_false = function
  | True -> True
  | False -> False
  | Lit symbol -> Lit symbol
  | Not p ->
    begin
      match simplify_true_false p with
      | True -> False
      | False -> True
      | Not p' -> p'
      | p' -> Not(p')
    end
  | And (p1, p2) ->
    begin
      match simplify_true_false p1, simplify_true_false p2 with
      | False, _ -> False
      | _, False -> False
      | True, p2' -> p2'
      | p1', True -> p1'
      | p1', p2' -> And(p1', p2')
    end
  | Or (p1, p2) ->
    begin
      match simplify_true_false p1, simplify_true_false p2 with
      | True, _ -> True
      | _, True -> True
      | False, p2' -> p2'
      | p1', False -> p1'
      | p1', p2' -> Or(p1', p2')
    end
  | Implies (p1, p2) ->
    begin
      match simplify_true_false p1, simplify_true_false p2 with
      | False, _ -> True
      | True, p2' -> p2'
      | p1', p2' -> Implies(p1', p2')
    end
  | Iff (p1, p2) ->
    begin
      match simplify_true_false p1, simplify_true_false p2 with
      | True, p2' -> p2'
      | p1', True -> p1'
      | False, p2' ->
        begin (* need to simplify Not(p2'), which depends on the form of p2' *)
          match p2' with
          | True -> False
          | False -> True
          | Not p -> p
          | p -> Not(p)
        end
      | p1', False ->
        begin (* need to simplify Not(p1') *)
          match p1' with
          | True -> False
          | False -> True
          | Not p -> p
          | p -> Not(p)
        end
      | p1', p2' -> Iff(p1', p2')
    end

(* Finds a symbol in a list of substitutions and returns the value to substitute for that symbol *)
let rec replace sub symbol =
  match sub with
  | [] -> Lit symbol
  | (s, value) :: sub -> if s = symbol then value else replace sub symbol

(* Substitutes propositions for symbols as specified by the sub argument *)
let rec substitute (prop : proposition) (sub : substitution) : proposition =
  match prop with
  | True -> True
  | False -> False
  | Lit s -> replace sub s
  | Not p -> Not (substitute p sub)
  | And(p1, p2) -> And(substitute p1 sub, substitute p2 sub)
  | Or(p1, p2) -> Or(substitute p1 sub, substitute p2 sub)
  | Implies(p1, p2) -> Implies(substitute p1 sub, substitute p2 sub)
  | Iff(p1, p2) -> And(substitute p1 sub, substitute p2 sub)

(* Converts a proposition to negation normal form *)
let to_nnf prop =
  let rec to_nnf = function
    | True -> True
    | False -> False
    | Lit symbol -> Lit symbol
    | Not p ->
      begin
        match p with
        | True -> False
        | False -> True
        | Lit s -> Not(Lit s)
        | Not p' -> to_nnf p'
        | And(p1, p2) -> Or(to_nnf (Not(p1)), to_nnf (Not(p2))) (* De Morgan's Laws *)
        | Or(p1, p2) -> And(to_nnf (Not(p1)), to_nnf (Not(p2)))
        | Implies(p1, p2) -> And(to_nnf p1, to_nnf (Not(p2))) (* not (A -> B) = A and not B*)
        | Iff(p1, p2) -> Or(And(to_nnf p1, to_nnf (Not(p2))), And(to_nnf (Not(p1)), to_nnf p2))
      end
    | And(p1, p2) -> And(to_nnf p1, to_nnf p2)
    | Or(p1, p2) -> Or(to_nnf p1, to_nnf p2)
    | Implies(p1, p2) -> to_nnf (Or(Not(p1), p2))
    | Iff(p1, p2) -> to_nnf (And(Implies(p1, p2), Implies(p2, p1)))
  in
  to_nnf (simplify_true_false prop)

(* Applies an interpretation to a proposition and returns the resulting proposition *)
let interpret prop i = simplify_true_false (substitute prop (sub_of_interpr i))

(* Tests whether a proposition holds under a given interpretation *)
let test_interpretation prop i =
  match interpret prop i with
  | True -> Some true
  | False -> Some false
  | _ -> None (* return None if cannot be determined *)
