(* Find a satisfying interpretation via brute force *)

open Types
open Lib_utils

(* lazy list *)
type 'a seq = Nil | Cons of 'a * (unit -> 'a seq)

let head = function
  | Nil -> None
  | Cons(x, _) -> Some x

let tail = function
  | Nil -> None
  | Cons(_, tl) -> Some tl

let rec get k s =
  match k, s with
  | 0, _ -> []
  | _, Nil -> []
  | k, Cons(x, f) -> x :: get (k - 1) (f ())

(* Find all the literals in a formula *)
let extract_symbols (prop : proposition) =
  let rec search prop acc =
    match prop with
    | True -> acc
    | False -> acc
    | Lit s -> Lit s :: acc
    | Not p -> search p acc
    | And(p1, p2) -> search p1 (search p2 acc)
    | Or(p1, p2) -> search p1 (search p2 acc)
    | Implies(p1, p2) -> search p1 (search p2 acc)
    | Iff(p1, p2) -> search p1 (search p2 acc)
  in
  search prop []

(* Generate all possible truth assignments for a list of symbols *)
let rec generate_truth_vals symbols =
  let rec prepend_all x llist = (* prepend x to each item in the lazy list *)
    match llist with
    | Nil -> Nil
    | Cons(xs, fn) -> Cons(x::xs, fun () -> prepend_all x (fn ()))
  in
  let rec interleave s1 s2 =
    match s1, s2 with
    | _, Nil -> s1
    | Nil, _ -> s2
    | Cons(x1, f1), _ -> Cons(x1, fun () -> interleave s2 (f1 ()))
  in
  match symbols with
  | [] -> Cons ([], fun () -> Nil)
  | Lit s :: tl ->
      let tl_vals = generate_truth_vals tl in
      let s_true = prepend_all (s, true) tl_vals in
      let s_false = prepend_all (s, false) tl_vals in
      interleave s_true s_false
  | _ -> failwith "discovered a bug"

(* brute force search for a satisfying interpretation *)
let satisfy prop =
  let symbols = extract_symbols prop in
  let truth_vals = generate_truth_vals symbols in
  let rec try_next truth_vals =
    match truth_vals with
    | Nil -> None
    | Cons(i, f) ->
      begin
        match test_interpretation prop i with
        | Some true -> Some i
        | _ -> try_next (f ())
      end
  in
  try_next truth_vals
