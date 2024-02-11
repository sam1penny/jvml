(* changes to typed_ast.ml
   - Add TupleGet, ConstructorGet, Switch and Match_Failure
   - Drop locations
   - Add explicit tag index to DeclConstr and con
*)
open Typing
open Printf
open Common

type loc = Lexing.position * Lexing.position
type con = IntCon of int | AdtCon of string * int

type expr =
  | Int of int
  | Ident of Typed_ast.type_expr * string
  | Bool of bool
  | Unit
  | Bop of Typed_ast.type_expr * expr * Common.bop * expr
  | If of Typed_ast.type_expr * expr * expr * expr
  | Fun of Typed_ast.type_expr * Typed_ast.type_expr * string * expr
  | App of Typed_ast.type_expr * expr * expr
  | Tuple of Typed_ast.type_expr * expr list
  | Let of Typed_ast.type_expr * string * expr * expr
  | LetRec of Typed_ast.type_expr * string * expr * expr
  | Constr of Typed_ast.type_expr * string
  | Seq of Typed_ast.type_expr * expr list
  | TupleGet of Typed_ast.type_expr * int * expr
  | ConstructorGet of Typed_ast.type_expr * expr
  (* switch branch_var, constructor + decision list, fallback_opt *)
  | Switch of Typed_ast.type_expr * expr * (con * expr) list * expr option
  | Match_Failure

type type_constr = DeclConstr of string * int * Typed_ast.type_expr option

type decl =
  | Val of Typed_ast.type_expr * string * expr
  | ValRec of Typed_ast.type_expr * string * expr
  | Type of Typed_ast.type_expr * string list * string * type_constr list

let desugared_tvar_cnter =
  let n = ref 0 in
  fun () ->
    let x = !n in
    n := x + 1;
    "match_failure_tvar" ^ string_of_int x

let get_expr_type = function
  | Int _ -> Typed_ast.TyInt
  | Ident (t, _) -> t
  | Bool _ -> Typed_ast.TyBool
  | Unit -> Typed_ast.TyUnit
  | Bop (t, _, _, _) -> t
  | If (t, _, _, _) -> t
  | Fun (t0, t1, _, _) -> Typed_ast.TyFun (t0, t1)
  | App (t, _, _) -> t
  | Tuple (t, _) -> t
  | Let (t, _, _, _) | LetRec (t, _, _, _) -> t
  | Constr (t, _) -> t
  | Seq (t, _) -> t
  | TupleGet (t, _, _) -> t
  | ConstructorGet (t, _) -> t
  | Switch (t, _, _, _) -> t
  | Match_Failure -> TyVar (desugared_tvar_cnter ())
(* huge bodge to get match_failure working

   todo - think of an alternative to get this working
*)

(* printing *)

let string_of_expr_node =
  let open Printf in
  function
  | Int i -> sprintf "Int %i" i
  | Bool b -> sprintf "Bool %b" b
  | Ident (ty, ident) -> sprintf "Ident %s : %s" ident (Typed_ast.pp_texpr ty)
  | Unit -> "()"
  | Bop (return_ty, _, op, _) ->
      sprintf "Bop %s : %s" (show_bop op) (Typed_ast.pp_texpr return_ty)
  | If _ -> "If"
  | Fun (arg_type, return_type, x, _) ->
      sprintf "Fun %s : %s" x
        (Typed_ast.pp_texpr (TyFun (arg_type, return_type)))
  | App _ -> "App"
  | Tuple (ty, _) -> sprintf "Tuple : %s" (Typed_ast.pp_texpr ty)
  | Let (_, x, _, _) -> sprintf "Let %s" x
  | LetRec (_, x, _, _) -> sprintf "LetRec %s" x
  | Constr (ty, cname) ->
      sprintf "Constructor %s : %s" cname (Typed_ast.pp_texpr ty)
  | Seq _ -> "Seq"
  | TupleGet (_, i, _) -> sprintf "Get %i" i
  | ConstructorGet _ -> "GetArg"
  | Switch _ -> "Switch"
  | Match_Failure -> "Match_Failure"

let pp_con ?(indent = "") con =
  match con with
  | IntCon i -> printf "%s└──Int(%i)\n" indent i
  | AdtCon (cname, tag) -> printf "%s└──%s : tag=%i\n" indent cname tag

let rec pp_expr ?(indent = "") expr =
  let open Printf in
  let pp_node n = printf "%s└──%s\n" indent (string_of_expr_node n) in
  let pp_rec_expr = pp_expr ~indent:(indent ^ "   ") in
  match expr with
  | Int _ | Bool _ | Ident _ | Unit | Constr _ -> pp_node expr
  | Bop (_, e0, _, e1) ->
      pp_node expr;
      pp_rec_expr e0;
      pp_rec_expr e1
  | If (_, e0, e1, e2) ->
      pp_node expr;
      pp_rec_expr e0;
      pp_rec_expr e1;
      pp_rec_expr e2
  | Fun (_, _, _, e) ->
      pp_node expr;
      pp_rec_expr e
  | App (_, e0, e1) ->
      pp_node expr;
      pp_rec_expr e0;
      pp_rec_expr e1
  | Tuple (_, es) ->
      pp_node expr;
      List.iter pp_rec_expr es
  | Let (_, _, e0, e1) ->
      pp_node expr;
      pp_rec_expr e0;
      pp_rec_expr e1
  | LetRec (_, _, e0, e1) ->
      pp_node expr;
      pp_rec_expr e0;
      pp_rec_expr e1
  | Seq (_, es) ->
      pp_node expr;
      List.iter pp_rec_expr es
  | TupleGet (_, _, e) ->
      pp_node expr;
      pp_rec_expr e
  | ConstructorGet (_, e) ->
      pp_node expr;
      pp_rec_expr e
  | Switch (_, e, cases, fallback) -> (
      pp_node expr;
      pp_rec_expr e;
      let case_indent = indent ^ "   " in
      List.iter (fun c -> pp_case case_indent c) cases;
      match fallback with
      | None -> ()
      | Some expr ->
          printf "%s└── <fallback>\n" case_indent;
          pp_expr ~indent:(case_indent ^ "   ") expr)
  | Match_Failure -> pp_node expr

and pp_case indent (pattern, expr) =
  printf "%s└── <case>\n" indent;
  let indent' = indent ^ "   " in
  pp_con ~indent:indent' pattern;
  pp_expr ~indent:indent' expr

let pp_tconstr ?(indent = "") =
  let open Printf in
  function
  | DeclConstr (cname, tag, None) ->
      printf "%s└──%s : tag=%i\n" indent cname tag
  | DeclConstr (cname, tag, Some texpr) ->
      printf "%s└──%s of %s : tag=%i\n" indent cname (Typed_ast.pp_texpr texpr)
        tag

let pp_decl ?(indent = "") =
  let open Printf in
  let print_with_indent = printf "%s└──%s\n" in
  function
  | Val (_, v, e) ->
      print_with_indent indent ("Val " ^ v);
      pp_expr ~indent:(indent ^ "   ") e
  | ValRec (_, v, e) ->
      print_with_indent indent ("ValRec " ^ v);
      pp_expr ~indent:(indent ^ "   ") e
  | Type (_, params, t, constructors) ->
      print_with_indent indent ("Type " ^ t);
      print_with_indent (indent ^ "   ")
        (String.concat "," params |> sprintf "params = [%s]");
      print_with_indent (indent ^ "   ") "constructors";
      List.iter (pp_tconstr ~indent:(indent ^ "      ")) constructors
