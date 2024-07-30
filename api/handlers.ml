(* Handlers for the different endpoints *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Power_prover.Exports

type post_request_data = {
  data : string;
} [@@deriving yojson]

type interpretation = {
  assignment : (string * bool) list option;
} [@@deriving yojson]

let get_data d = d.data

let health_endpoint _ =
  {|{"status": "pass"}|}
  |> Dream.json

let validate_formula_endpoint request = 
  let%lwt body = Dream.body request in

  let input = 
    body
    |> Yojson.Safe.from_string
    |> post_request_data_of_yojson
    |> get_data
  in
  
  try
    input
    |> parse_input
    |> Power_prover.To_string.string_of_prop
    |> fun s -> "{\"is_formula\": true, \"formula\": \"" ^ s ^ "\"}"
    |> Dream.json
  
  with _ -> (* TODO: 400 Bad Request, error message *)
    "{\"is_formula\": false, \"formula\": \"" ^ input ^ "\"}"
    |> Dream.json

let satisfy_endpoint request = 
  let%lwt body = Dream.body request in
  
  let input =
    body
    |> Yojson.Safe.from_string
    |> post_request_data_of_yojson
    |> get_data
    |> parse_input
    |> find_satisfying_interpretation
  in

  { assignment = input; }
  |> yojson_of_interpretation
  |> Yojson.Safe.to_string
  |> Dream.json

let sequent_proof_endpoint request =
  let%lwt body = Dream.body request in

  let seq_proof =
    body
    |> Yojson.Safe.from_string
    |> post_request_data_of_yojson
    |> get_data
    |> parse_input
    |> get_sequent_calculus_proof
  in

  let json_string =
    begin
      match seq_proof with
      | None -> {|{"is_provable": "false"}|}
      | Some tree ->
          let s = Power_prover.To_string.json_string_of_stree tree in
          {|{"is_provable": "true", "sequent_proof": |} ^ s ^ "}"
    end
  in

  json_string
  |> Dream.json
