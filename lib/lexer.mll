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

rule token = parse
    | white {token lexbuf}
    | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | '+' { ADD }
    | '-' { SUB }
    | '*' { MUL }
    | '/' { DIV }
    | eof { EOF }
    | _  as c { raise (SyntaxError ("Unknown Character: " ^ (String.make 1 c)))}