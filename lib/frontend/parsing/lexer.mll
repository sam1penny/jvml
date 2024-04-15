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
let float = int '.' int
let letter = ['a'-'z' 'A'-'Z']
let lowercase_indent = (['a'-'z'] | '_') (letter | ['0'-'9'] | '_' | '\'')*
let uppercase_ident = (['A'-'Z'] | '_') (letter | ['0'-'9'] | '_' | '\'')*
let any_string_without_quote = "[^\"]*"

rule token = parse
    | white {token lexbuf}
    | int { INT (Int32.of_string (Lexing.lexeme lexbuf)) }
    | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
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
    | '\"' { QUOTATION_MARK }
    | "^" { STRING_CONCAT }
    | '=' { EQ }
    | '<' { LT }
    | '>' { GT }
    | "<=" { LEQ }
    | ">=" { GEQ }
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
    | lowercase_indent { LOWERCASE_IDENT (Lexing.lexeme lexbuf)}
    | uppercase_ident { UPPERCASE_IDENT (Lexing.lexeme lexbuf)}
    | any_string_without_quote { ANY_STRING (Lexing.lexeme lexbuf )}
    | eof { EOF }
    | '\n' { next_line lexbuf; token lexbuf }
    | _  as c { raise (SyntaxError ("Unknown Character: " ^ (String.make 1 c)))}