{
open Lexing
open Parser

exception SyntaxError of string

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
let letter = ['a'-'z' 'A'-'Z']
let lowercase_indent = (['a'-'z'] | '_') (letter | ['0'-'9'] | '_' | '\'')*
let uppercase_ident = (['A'-'Z'] | '_') (letter | ['0'-'9'] | '_' | '\'')*

rule token = parse
    | white {token lexbuf}
    | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | '+' { ADD }
    | '-' { SUB }
    | '*' { MUL }
    | '/' { DIV }
    | '=' { EQ }
    | '<' { LT }
    | '>' { GT }
    | "&&" { AND }
    | "||" { OR }
    | "()" { UNIT }
    | '|' { BAR }
    | "->" { ARROW }
    | '_' { UNDERSCORE }
    | ',' { COMMA }
    | '\'' { APOSTROPHE }
    | "::" { CONS }
    | "[]" { EMPTY_LIST }
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
    | "in" { IN }
    | lowercase_indent { LOWERCASE_IDENT (Lexing.lexeme lexbuf)}
    | uppercase_ident { UPPERCASE_IDENT (Lexing.lexeme lexbuf)}
    | eof { EOF }
    | '\n' { next_line lexbuf; token lexbuf }
    | _  as c { raise (SyntaxError ("Unknown Character: " ^ (String.make 1 c)))}