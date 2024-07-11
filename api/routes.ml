(* Define the routes for our api *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Power_prover.Exports

type post_request_data = {
  data : string;
} [@@deriving yojson]

type interpretation = {
  assignment : (string * bool) list option;
} [@@deriving yojson]

let routes = [

  Dream.post "/"
    (fun request ->
      let%lwt body = Dream.body request in

      let _ = (* post_request_data *)
        body
        |> Yojson.Safe.from_string
        |> post_request_data_of_yojson
      in

      `String "Dummy return data"
      |> Yojson.Safe.to_string
      |> Dream.json);

  Dream.post "/satisfy"
    (fun request ->
      let%lwt body = Dream.body request in
      
      let input =
        body
        |> Yojson.Safe.from_string
        |> post_request_data_of_yojson
      in
      
      let i = input.data |> parse_input |> find_satisfying_interpretation in

      let response = {
        assignment = i;
      } in

      response |> yojson_of_interpretation |> Yojson.Safe.to_string |> Dream.json
      
    );

]
