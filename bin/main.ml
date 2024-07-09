(* open Ppx_yojson_conv_lib.Yojson_conv.Primitives *)
open Power_prover.Types
open Power_prover.Parsing.Parser

let () =
  let _ = parse [LIT "a"; OR; LIT "b"] in ()

(* type post_request_data = {
  data : string;
} [@@deriving yojson]


let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.origin_referrer_check
  @@ Dream.router [

    Dream.post "/"
      (fun request ->
        let%lwt body = Dream.body request in

        let post_request_data =
          body
          |> Yojson.Safe.from_string
          |> post_request_data_of_yojson
        in

        let _ = parse post_request_data.data in

        `String "Dummy return data"
        |> Yojson.Safe.to_string
        |> Dream.json);

  ] *)
  