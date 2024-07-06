let successes = ref 0
let failures = ref 0

let middleware inner_handler request =
  try%lwt
    let%lwt response = inner_handler request in
    successes := !successes + 1;
    Lwt.return response
  
  with exn ->
    failures := !failures + 1;
    raise exn
  
let () =
  Dream.run
  @@ Dream.logger
  @@ middleware
  @@ Dream.router [

    Dream.get "/failures"
      (fun _ ->
        raise (Failure "Webapp failed!")
      );

    Dream.get "/info"
      (fun _ ->
        Dream.html (Printf.sprintf
          "Number of successes: %3i<br>Number of failures: %3i"
          !successes
          !failures
        )
      );

  ]
