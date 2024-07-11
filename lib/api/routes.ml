(* Define the routes for our api *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type post_request_data = {
  data : string;
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

]
