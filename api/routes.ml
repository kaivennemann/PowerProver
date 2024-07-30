(* Define the routes for our api *)

open Handlers

let routes = [

  Dream.get "/health"
    health_endpoint;

  Dream.post "/validate_formula"
    validate_formula_endpoint;

  Dream.post "/satisfy"
    satisfy_endpoint;
  
  Dream.post "/sequent_proof"
    sequent_proof_endpoint;
    
  (* /tableau_calculus_proof *)

]
