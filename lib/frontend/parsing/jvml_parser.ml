open Parsed_ast

exception CustomParserError of loc * string

let rec check_letrec = function
  | (Int _ | Float _ | String _ | Ident _ | Constr _ | Bool _ | Unit _) as e ->
      e
  | Bop (loc, e0, op, e1) -> Bop (loc, check_letrec e0, op, check_letrec e1)
  | If (loc, e0, e1, e2) ->
      If (loc, check_letrec e0, check_letrec e1, check_letrec e2)
  | Fun (loc, x, e) -> Fun (loc, x, check_letrec e)
  | App (loc, e0, e1) -> App (loc, check_letrec e0, check_letrec e1)
  | Match (loc, e, cases) ->
      Match
        (loc, check_letrec e, List.map (fun (p, e) -> (p, check_letrec e)) cases)
  | Tuple (loc, es) -> Tuple (loc, List.map check_letrec es)
  | Let (loc, x, e0, e1) -> Let (loc, x, check_letrec e0, check_letrec e1)
  | LetRec (loc, x, (Fun _ as e0), e1) ->
      LetRec (loc, x, check_letrec e0, check_letrec e1)
  | LetRec (loc, _, _, _) ->
      raise
      @@ CustomParserError
           (loc, "You must bind a lambda on the rhs of a 'let rec'")
  | Seq (loc, exprs) -> Seq (loc, List.map check_letrec exprs)

let map_over_decl_exprs f = function
  | Val (loc, x, e) -> Val (loc, x, f e)
  | ValRec (loc, x, e) -> ValRec (loc, x, e)
  | Type _ as t -> t

let check_valrec = function
  | ValRec (_, _, Fun _) as decl -> decl
  | ValRec (loc, _, _) ->
      raise
      @@ CustomParserError
           (loc, "You must bind a lambda on the rhs of a 'val rec'")
  | decl -> decl

module Wrapped_parser = Nice_parser.Make (struct
  type result = Parsed_ast.decl list
  type token = Parser.token

  exception ParseError = Parser.Error

  let parse = Parser.prog

  include Lexer
end)
