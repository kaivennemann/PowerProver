(* Define the routes for our api *)

open Handlers

let routes = [

  Dream.get "/health"
    health_endpoint;

  Dream.post "/validate_formula"
    validate_formula_endpoint;

  Dream.post "/satisfy"
    satisfy_endpoint;

]
