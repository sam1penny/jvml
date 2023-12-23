type expr =
  | Int of int
  | Ident of type_expr * string
  | Bool of bool
  | Unit
  | Bop of type_expr * expr * Common.bop * expr
  | If of type_expr * expr * expr * expr
  | Fun of type_expr * string * expr
  | App of type_expr * expr * expr
  | Match of type_expr * expr * (Parsing.Parsed_ast.pattern * expr) list
  | Tuple of type_expr * expr list
  | Let of type_expr * string * expr * expr

(*
and pattern =
  | Pat_Int of int
  | Pat_Ident of string
  | Pat_Bool of bool
  | Pat_Unit
  | Pat_Any
  | Pat_Or of pattern * pattern
  | Pat_Tuple of type_expr * pattern list
  | Pat_Constr of type_expr * string * pattern option
*)
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
      List.map
        (fun t ->
          match t with TyFun _ -> "(" ^ ty_repr t ^ ")" | _ -> ty_repr t)
        ts
      |> String.concat " * " |> Printf.sprintf "(%s)"
  | TyFun (f, c) -> (
      (match f with TyFun _ -> "(" ^ ty_repr f ^ ")" | _ -> ty_repr f)
      ^ " -> "
      ^ match c with TyFun _ -> "" ^ ty_repr c ^ "" | _ -> ty_repr c)

let bop_arg_type =
  let open Common in
  function
  | ADD | SUB | MUL | DIV | LT | GT -> TyInt
  | AND | OR -> TyBool
  | EQ -> raise (Invalid_argument "idk about eq yet - polymorphic?")

let bop_return_type =
  let open Common in
  function
  | ADD | SUB | MUL | DIV -> TyInt
  | AND | OR | LT | GT -> TyBool
  | EQ -> raise (Invalid_argument "idk about eq yet - polymorphic?")
