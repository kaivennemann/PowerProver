(* Sanitize the inputs *)

(* TODO: handle exceptions *)
let process (input : string) : Types.proposition =
  input |> Lexing.Lexer.lex |> Parsing.Parser.parse

