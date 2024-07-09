open Power_prover.To_string
open Power_prover.Lexing.Lexer

let _ = 
  let tokens = lex "a|b   c && d    i love sushi&!!!" in
  let s = string_of_tokens tokens in
  print_endline s
