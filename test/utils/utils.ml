open Common

type type_expr =
  | TyInt
  | TyBool
  | TyUnit
  | TyCustom of type_expr list * string
  | TyVar of string
  | TyTuple of type_expr list
  | TyFun of type_expr * type_expr

type expr =
  | Int of int
  | Ident of string
  | Constr of string
  | Bool of bool
  | Unit
  | Bop of expr * bop * expr
  | If of expr * expr * expr
  | Fun of string * expr
  | App of expr * expr
  | Match of expr * (pattern * expr) list
  | Tuple of expr list
  | Let of string * expr * expr
  | LetRec of string * expr * expr
  | Seq of expr list

and pattern =
  | Pat_Int of int
  | Pat_Ident of string
  | Pat_Bool of bool
  | Pat_Unit
  | Pat_Any
  | Pat_Or of pattern * pattern
  | Pat_Tuple of pattern list
  | Pat_Constr of string * pattern option

type type_constr = DeclConstr of string * type_expr option

type decl =
  | Val of string * expr
  | ValRec of string * expr
  | Type of string list * string * type_constr list

(* simplify dummy location for testing *)

let dummy = (Lexing.dummy_pos, Lexing.dummy_pos)

let rec add_dummy_loc_texpr =
  let open Parsing in
  function
  | TyInt -> Parsed_ast.TyInt dummy
  | TyBool -> Parsed_ast.TyBool dummy
  | TyUnit -> Parsed_ast.TyUnit dummy
  | TyCustom (texprs, tname) ->
      Parsed_ast.TyCustom (dummy, List.map add_dummy_loc_texpr texprs, tname)
  | TyVar v -> Parsed_ast.TyVar (dummy, v)
  | TyTuple ts -> Parsed_ast.TyTuple (dummy, List.map add_dummy_loc_texpr ts)
  | TyFun (t0, t1) ->
      Parsed_ast.TyFun (dummy, add_dummy_loc_texpr t0, add_dummy_loc_texpr t1)

let rec add_dummy_loc_expr =
  let open Parsing in
  function
  | Int i -> Parsed_ast.Int (dummy, i)
  | Ident ident -> Parsed_ast.Ident (dummy, ident)
  | Constr c -> Parsed_ast.Constr (dummy, c)
  | Bool b -> Parsed_ast.Bool (dummy, b)
  | Unit -> Parsed_ast.Unit dummy
  | Bop (e0, op, e1) ->
      Parsed_ast.Bop (dummy, add_dummy_loc_expr e0, op, add_dummy_loc_expr e1)
  | If (e0, e1, e2) ->
      Parsed_ast.If
        ( dummy,
          add_dummy_loc_expr e0,
          add_dummy_loc_expr e1,
          add_dummy_loc_expr e2 )
  | Fun (x, e) -> Parsed_ast.Fun (dummy, x, add_dummy_loc_expr e)
  | App (e0, e1) ->
      Parsed_ast.App (dummy, add_dummy_loc_expr e0, add_dummy_loc_expr e1)
  | Match (e, cases) ->
      Parsed_ast.Match
        ( dummy,
          add_dummy_loc_expr e,
          List.map
            (fun (p, e) -> (add_dummy_loc_pat p, add_dummy_loc_expr e))
            cases )
  | Tuple es -> Parsed_ast.Tuple (dummy, List.map add_dummy_loc_expr es)
  | Let (x, e0, e1) ->
      Parsed_ast.Let (dummy, x, add_dummy_loc_expr e0, add_dummy_loc_expr e1)
  | LetRec (x, e0, e1) ->
      Parsed_ast.LetRec (dummy, x, add_dummy_loc_expr e0, add_dummy_loc_expr e1)
  | Seq es -> Parsed_ast.Seq (dummy, List.map add_dummy_loc_expr es)

and add_dummy_loc_pat =
  let open Parsing in
  function
  | Pat_Int i -> Parsed_ast.Pat_Int (dummy, i)
  | Pat_Ident ident -> Parsed_ast.Pat_Ident (dummy, ident)
  | Pat_Bool b -> Parsed_ast.Pat_Bool (dummy, b)
  | Pat_Unit -> Parsed_ast.Pat_Unit dummy
  | Pat_Any -> Parsed_ast.Pat_Any dummy
  | Pat_Or (p0, p1) ->
      Parsed_ast.Pat_Or (dummy, add_dummy_loc_pat p0, add_dummy_loc_pat p1)
  | Pat_Tuple ps -> Parsed_ast.Pat_Tuple (dummy, List.map add_dummy_loc_pat ps)
  | Pat_Constr (c, None) -> Parsed_ast.Pat_Constr (dummy, c, None)
  | Pat_Constr (c, Some p) ->
      Parsed_ast.Pat_Constr (dummy, c, Some (add_dummy_loc_pat p))

let add_dummy_loc_constr =
  let open Parsing in
  function
  | DeclConstr (name, None) -> Parsed_ast.DeclConstr (dummy, name, None)
  | DeclConstr (name, Some texpr) ->
      Parsed_ast.DeclConstr (dummy, name, Some (add_dummy_loc_texpr texpr))

let add_dummy_loc_decl =
  let open Parsing in
  function
  | Val (x, e) -> Parsed_ast.Val (dummy, x, add_dummy_loc_expr e)
  | ValRec (x, e) -> Parsed_ast.ValRec (dummy, x, add_dummy_loc_expr e)
  | Type (params, tname, constructors) ->
      Parsed_ast.Type
        (dummy, params, tname, List.map add_dummy_loc_constr constructors)

let pp_tree_result = function
  | Ok ty -> "Ok(" ^ Typing.Typed_ast.pp_texpr ty ^ ")"
  | Error _ -> "Error"
