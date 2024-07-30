
(* Middleware to enable CORS for testing purposes *)
let cors_middleware handler request =
  let handlers =
    [
      "Allow", "OPTIONS, GET, HEAD, POST";
      "Access-Control-Allow-Origin", "*";
      "Access-Control-Allow-Methods", "OPTIONS, GET, HEAD, POST";
      "Access-Control-Allow-Headers", "Content-Type";
      "Access-Control-Max-Age", "86400"
    ]
  in

  let%lwt res = handler request in
  handlers
  |> List.map (fun (key, value) -> Dream.add_header res key value)
  |> ignore;
  Lwt.return res

  (* Create and start the app *)
let startapp () =
  Dream.run
  @@ Dream.logger
  (* @@ Dream.origin_referrer_check *)
  @@ cors_middleware (* TODO: remove *)
  @@ Dream.router Routes.routes
