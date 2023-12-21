let parse_file filename =
  let in_channel = In_channel.open_text filename in
  let lexbuf = Lexing.from_channel in_channel in
  Lexing.set_filename lexbuf filename;
  let parse_tree = Parser.prog Lexer.token lexbuf in
  In_channel.close in_channel;
  parse_tree

let parse_string s =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.token lexbuf
