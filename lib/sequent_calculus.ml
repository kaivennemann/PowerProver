
(* Sequent Calculus *)

open Datatypes
open To_string


type seq = Seq of proposition list * proposition list;; (* sequent *)

type stree = (* tree of sequents *)
  | Taut (* tautology of the form A, Gamma => A, Delta *)
  | Br of stree * stree

let string_of_seq (Seq (p1, p2)) =
  let rec string_of_prop_list = function
    | [] -> ""
    | p :: ps -> (string_of_prop p) ^ ", " ^ (string_of_prop_list ps)
  in
  (string_of_prop_list p1) ^ "⇒" ^ (string_of_prop_list p2);;

(* let rec reduce_seq s =
  let rec not_lhs (Seq(p1s, p2s)) =
    match p1s with
    | [] -> Seq(p1s, p2s), None, false
    | p::ps ->
      (match p with
      | Not(p') -> Seq(ps, p'::p2s), None, true
      | _ -> let Seq(ps', p2s), _, found = not_lhs (Seq(ps, p2s)) in Seq(p::ps', p2s), None, found)
  in
  let rec not_rhs (Seq(p1s, p2s)) =
    match p2s with
    | [] -> Seq(p1s, p2s), None, false
    | p::ps ->
      (match p with
      | Not(p') -> Seq(p'::p1s, ps), None, true
      | _ -> let Seq(p1s, ps'), _, found = not_rhs (Seq(p1s, ps)) in Seq(p1s, p::ps'), None, found)
  in
  let rec and_lhs (Seq(p1s, p2s)) =
    match p1s with
    | [] -> Seq(p1s, p2s), None, false
    | p::ps ->
      (match p with
      | And(p1, p2) -> Seq(p1::p2::ps, p2s), None, true
      | _ -> let Seq(p1s, ps'), _, found = and_lhs (Seq(p1s, ps)) in Seq(p1s, p::ps'), None, found)
  in
  let rec and_rhs (Seq(p1s, p2s)) =
    match p2s with
    | [] -> Seq(p1s, p2s), None, false
    | p::ps ->
      (match p with
      | And(p1, p2) -> Seq(p1s, p1::ps), Some (Seq(p1s, p2::ps)), true
      | _ ->
        (match and_rhs (Seq(p1s, ps)) with
        | Seq(p1s, ps'), None, found -> Seq(p1s, p2s), None, false
        | Seq(p1s, ps'), Some (Seq(p1s', ps'')), found -> Seq(p1s, p::ps'), Some (Seq(p1s', p::ps'')), true))
  in
  match s with
  | Seq  *)

  (*
  Steps (repeat in this order):
  - reduce AND on LHS
  - reduce OR on RHS
  - reduce NOT on LHS
  - reduce NOT on RHS
  - reduce -> (RHS)
  - reduce <-> (both)
  - reduce OR (LHS)
  - reduce AND (RHS)
  - reduce -> (LHS)
  *)
  