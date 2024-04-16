{
open Lexing
open Parser

exception LexError of string

(* copied from Real World Ocaml / Compiler-Construction course *)
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let white = [' ' '\t']+
let int = '-'? ['0'-'9']+
let float = int '.' int
let letter = ['a'-'z' 'A'-'Z']
let lowercase_indent = (['a'-'z'] | '_') (letter | ['0'-'9'] | '_' | '\'')*
let uppercase_ident = (['A'-'Z'] | '_') (letter | ['0'-'9'] | '_' | '\'')*

rule next_token = parse
    | white {next_token lexbuf}
    | int { INT (Int32.of_string (Lexing.lexeme lexbuf)) }
    | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | '"'   { read_string (Buffer.create 17) lexbuf }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | '+' { ADD }
    | '-' { SUB }
    | '*' { MUL }
    | '/' { DIV }
    | "+." { FLOAT_ADD }
    | "-." { FLOAT_SUB }
    | "*." { FLOAT_MUL }
    | "/." { FLOAT_DIV }
    | "^" { STRING_CONCAT }
    | '=' { EQ }
    | '<' { LT }
    | '>' { GT }
    | "<=" { LEQ }
    | ">=" { GEQ }
    | "<." { FLOAT_LT }
    | ">." { FLOAT_GT }
    | "<=." { FLOAT_LEQ }
    | ">=." { FLOAT_GEQ }
    | "&&" { AND }
    | "||" { OR }
    | "()" { UNIT }
    | '|' { BAR }
    | "->" { ARROW }
    | '_' { UNDERSCORE }
    | ',' { COMMA }
    | '\'' { APOSTROPHE }
    | ';' { SEMICOLON }
    | '{' { LCURLY }
    | '}' { RCURLY }
    | "::" { CONS }
    | "[]" { EMPTY_LIST }
    | '['  { LSQUARE }
    | ']'  { RSQUARE }
    | "true" { TRUE }
    | "false" { FALSE }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "match" { MATCH }
    | "with" { WITH }
    | "fun" { FUN }
    | "int" { TINT }
    | "bool" { TBOOL }
    | "unit" { TUNIT }
    | "type" { TYPE }
    | "of" { OF }
    | "val" { VAL }
    | "let" { LET }
    | "rec" { REC }
    | "in" { IN }
    | "do" { DO }
    | "real" { REAL }
    | lowercase_indent { LOWERCASE_IDENT (Lexing.lexeme lexbuf)}
    | uppercase_ident { UPPERCASE_IDENT (Lexing.lexeme lexbuf)}
    | eof { EOF }
    | "(*" { comment lexbuf; next_token lexbuf }
    | '\n' { next_line lexbuf; next_token lexbuf }
    | _  as c { raise (LexError (Printf.sprintf "[lexer] unknown character: '%c'" c))}
(* copied from Real World OCaml *)
and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (LexError (Printf.sprintf "[lexer] illegal string character: %s" (Lexing.lexeme lexbuf))) }
  | eof { raise (LexError ("[lexer] string is not terminated")) }
(* copied from Compiler Construction *)
and comment = parse
  | "*)" { () }
  | '\n' { next_line lexbuf; comment lexbuf }
  | "(*" {comment lexbuf; comment lexbuf }
  | _ { comment lexbuf }