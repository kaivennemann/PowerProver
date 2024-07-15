(* Logic for applying sequent calculus rules *)

open Types
open Headers

(* apply NOT rule for LHS if possible *)
let rec not_lhs = function
  | Seq([], _) -> raise RuleNotApplicable
  | Seq(Not(p1) :: p1s, p2s) -> [Seq(p1s, p1 :: p2s)]
  | Seq(_ :: p1s, p2s) -> not_lhs (Seq(p1s, p2s))

(* apply NOT rule for RHS if possible *)
let rec not_rhs = function
  | Seq(_, []) -> raise RuleNotApplicable
  | Seq(p1s, Not(p2) :: p2s) -> [Seq(p2 :: p1s, p2s)]
  | Seq(p1s, _ :: p2s) -> not_rhs (Seq(p1s, p2s))

(* apply AND rule for LHS if possible *)
let rec and_lhs = function
  | Seq([], _) -> raise RuleNotApplicable
  | Seq(And(p1, p1') :: p1s, p2s) -> [Seq(p1 :: p1' :: p1s, p2s)]
  | Seq(_ :: p1s, p2s) -> and_lhs (Seq(p1s, p2s))

(* apply AND rule for RHS if possible *)
let rec and_rhs = function
  | Seq(_, []) -> raise RuleNotApplicable
  | Seq(p1s, And(p2, p2') :: p2s) -> [Seq(p1s, p2 :: p2s); Seq(p1s, p2' :: p2s)]
  | Seq(p1s, _ :: p2s) -> and_rhs (Seq(p1s, p2s))

(* apply OR rule for LHS if possible *)
let rec or_lhs = function
  | Seq([], _) -> raise RuleNotApplicable
  | Seq(Or(p1, p1') :: p1s, p2s) -> [Seq(p1 :: p1s, p2s); Seq(p1' :: p1s, p2s)]
  | Seq(_ :: p1s, p2s) -> or_lhs (Seq(p1s, p2s))

(* apply OR rule for RHS if possible *)
let rec or_rhs = function
  | Seq(_, []) -> raise RuleNotApplicable
  | Seq(p1s, Or(p2, p2') :: p2s) -> [Seq(p1s, p2 :: p2' :: p2s)]
  | Seq(p1s, _ :: p2s) -> or_rhs (Seq(p1s, p2s))

(* apply -> rule for LHS if possible *)
let rec implies_lhs = function
  | Seq([], _) -> raise RuleNotApplicable
  | Seq(Implies(p1, p1') :: p1s, p2s) -> [Seq(p1s, p1 :: p2s); Seq(p1' :: p1s, p2s)]
  | Seq(_ :: p1s, p2s) -> implies_lhs (Seq(p1s, p2s))

(* apply -> rule for RHS if possible *)
let rec implies_rhs = function
  | Seq(_, []) -> raise RuleNotApplicable
  | Seq(p1s, Implies(p2, p2') :: p2s) -> [Seq(p2 :: p1s, p2' :: p2s)]
  | Seq(p1s, _ :: p2s) -> implies_rhs (Seq(p1s, p2s))

(* precedence rules for applying rules *)
let rule_precedence_list = [ 
  and_lhs;        (* reduce AND on LHS *)
  or_rhs;         (* reduce OR on RHS  *)
  not_lhs;        (* reduce NOT on LHS *)
  not_rhs;        (* reduce NOT on RHS *)
  implies_rhs;    (* reduce -> on RHS  *)
  or_lhs;         (* reduce OR on LHS  *)
  and_rhs;        (* reduce AND on RHS *)
  implies_lhs     (* reduce -> on LHS  *)
]
