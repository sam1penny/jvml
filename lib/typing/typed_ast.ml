type expr =
  | Int of int
  | Ident of type_expr * string
  | Bool of bool
  | Unit
  | Oper of type_expr * expr * Parsing.Parsed_ast.oper * expr
  | If of type_expr * expr * expr * expr
  | Fun of type_expr * string * expr
  | App of type_expr * expr * expr
  | Match of type_expr * expr * (Parsing.Parsed_ast.pattern * expr) list
  | Tuple of type_expr * expr list
  | Let of type_expr * string * expr * expr

and pattern =
  | Pat_Int of int
  | Pat_Ident of string
  | Pat_Bool of bool
  | Pat_Unit
  | Pat_Any
  | Pat_Or of pattern * pattern
  | Pat_Tuple of type_expr * pattern list
  | Pat_Constr of type_expr * string * pattern option

and type_expr =
  | TyInt
  | TyBool
  | TyUnit
  | TyVar of string
  | TyTuple of type_expr list
  | TyFun of type_expr * type_expr

let rec ty_repr = function
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyUnit -> "unit"
  | TyVar v -> v
  | TyTuple ts ->
      List.map ty_repr ts |> String.concat "*" |> Printf.sprintf "(%s)"
  | TyFun (f, c) -> (
      (match f with TyFun _ -> "(" ^ ty_repr f ^ ")" | _ -> ty_repr f)
      ^ " -> "
      ^ match c with TyFun _ -> "" ^ ty_repr c ^ "" | _ -> ty_repr c)

let oper_arg_type =
  let open Parsing in
  function
  | Parsed_ast.ADD | Parsed_ast.SUB | Parsed_ast.MUL | Parsed_ast.DIV
  | Parsed_ast.LT | Parsed_ast.GT ->
      TyInt
  | Parsed_ast.AND | Parsed_ast.OR -> TyBool
  | Parsed_ast.EQ -> raise (Invalid_argument "idk about eq yet - polymorphic?")

let oper_return_type =
  let open Parsing in
  function
  | Parsed_ast.ADD | Parsed_ast.SUB | Parsed_ast.MUL | Parsed_ast.DIV -> TyInt
  | Parsed_ast.AND | Parsed_ast.OR | Parsed_ast.LT | Parsed_ast.GT -> TyBool
  | Parsed_ast.EQ -> raise (Invalid_argument "idk about eq yet - polymorphic?")
