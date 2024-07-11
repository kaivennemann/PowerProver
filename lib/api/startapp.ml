(* Create and start the app *)

let startapp () =
  Dream.run
  @@ Dream.logger
  @@ Dream.origin_referrer_check
  @@ Dream.router Routes.routes
