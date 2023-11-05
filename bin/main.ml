open Core

let parse s =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.token lexbuf

let () = parse "1+3/4 * 5" |> Ast.pretty_print; print_newline ()