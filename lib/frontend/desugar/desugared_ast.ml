(* changes to typed_ast.ml
   - Add TupleGet, ConstructorGet, Switch and Match_Failure
   - Drop locations
   - Add explicit tag index to DeclConstr and con
*)
open Typing
open Printf
open Common

type loc = Lexing.position * Lexing.position

type con =
  | IntCon of Int32.t
  | BoolCon of bool
  | UnitCon
  | AdtCon of string * Int32.t

type expr =
  | Int of Int32.t
  | Float of float
  | String of string
  | Ident of Typed_ast.type_expr * string
  | Bool of bool
  | Unit
  | Bop of Typed_ast.type_expr * expr * Common.bop * expr
  | Uop of Typed_ast.type_expr * Common.uop * expr
  | If of Typed_ast.type_expr * expr * expr * expr
  | Fun of Typed_ast.type_expr * Typed_ast.type_expr * string * expr
  | App of Typed_ast.type_expr * expr * expr
  | Direct_app of
      Typed_ast.type_expr
      * Typed_ast.type_expr list
      * Typed_ast.type_expr
      * string
      * expr list
  | Tuple of Typed_ast.type_expr * expr list
  | Let of Typed_ast.type_expr * string * expr * expr
  | LetRec of Typed_ast.type_expr * string * expr * expr
  | Constr of Typed_ast.type_expr * string
  | Seq of Typed_ast.type_expr * expr list
  | TupleGet of Typed_ast.type_expr * int * expr
  | ConstructorGet of Typed_ast.type_expr * string * expr
  (* switch branch_var, constructor + decision list, fallback_opt *)
  | Switch of Typed_ast.type_expr * expr * (con * expr) list * expr option
  | Match_Failure
  (* ref to expr, label (in compiled repr) *)
  | Shared_Expr of expr ref * string option ref * bool ref
  | While_true of expr
  | Break of expr
  (* Sequence of assignments, special case as assign does not produce a 'unit' on the stack *)
  | Assign_Seq of (string * Typed_ast.type_expr * expr) list
  | Hole
  (* e1[e0] = e2 *)
  | Set_Tuple of expr * expr * expr

type type_constr = DeclConstr of string * Int32.t * Typed_ast.type_expr option

type decl =
  | Val of Typed_ast.type_expr * string * expr
  | ValRec of Typed_ast.type_expr * string * expr
  | Type of Typed_ast.type_expr * string list * string * type_constr list
  (* mutual recursion as consequence of lambda lifting *)
  | And of decl list

let desugared_tvar_cnter =
  let n = ref 0 in
  fun () ->
    let x = !n in
    n := x + 1;
    "match_failure_tvar" ^ string_of_int x

let rec get_expr_type = function
  | Int _ -> Typed_ast.TyInt
  | Float _ -> Typed_ast.TyFloat
  | String _ -> Typed_ast.TyString
  | Ident (t, _) -> t
  | Bool _ -> Typed_ast.TyBool
  | Unit -> Typed_ast.TyUnit
  | Bop (t, _, _, _) -> t
  | Uop (t, _, _) -> t
  | If (t, _, _, _) -> t
  | Fun (t0, t1, _, _) -> Typed_ast.TyFun (t0, t1)
  | App (t, _, _) -> t
  | Direct_app (t, _, _, _, _) -> t
  | Tuple (t, _) -> t
  | Let (t, _, _, _) | LetRec (t, _, _, _) -> t
  | Constr (t, _) -> t
  | Seq (t, _) -> t
  | TupleGet (t, _, _) -> t
  | ConstructorGet (t, _, _) -> t
  | Switch (t, _, _, _) -> t
  | Match_Failure -> TyVar (desugared_tvar_cnter ())
  | Shared_Expr ({ contents = e }, _, _) -> get_expr_type e
  | While_true e -> get_expr_type e
  | Break e -> get_expr_type e
  | Assign_Seq _ -> Typed_ast.TyUnit
  | Hole -> raise @@ Failure "called get_expr_type on Hole!"
  | Set_Tuple _ -> Typed_ast.TyUnit

(* printing *)

let string_of_expr_node =
  let open Printf in
  function
  | Int i -> sprintf "Int %ld" i
  | Float f -> sprintf "Float %f" f
  | String s -> sprintf "String %s" s
  | Bool b -> sprintf "Bool %b" b
  | Ident (ty, ident) -> sprintf "Ident %s : %s" ident (Typed_ast.pp_texpr ty)
  | Unit -> "()"
  | Bop (return_ty, _, op, _) ->
      sprintf "Bop %s : %s" (show_bop op) (Typed_ast.pp_texpr return_ty)
  | Uop (return_ty, op, _) ->
      sprintf "Uop %s : %s" (show_uop op) (Typed_ast.pp_texpr return_ty)
  | If _ -> "If"
  | Fun (arg_type, return_type, x, _) ->
      sprintf "Fun %s : %s" x
        (Typed_ast.pp_texpr (TyFun (arg_type, return_type)))
  | App _ -> "App"
  | Direct_app (_, _, _, name, _) -> sprintf "Direct_app : %s" name
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
  | Shared_Expr (_, _, _) -> "Shared"
  | While_true _ -> "While true"
  | Break _ -> "Break"
  | Assign_Seq _ -> sprintf "Assign_Seq"
  | Hole -> "Hole"
  | Set_Tuple (_, _, _) -> "Set_Tuple"

let pp_con ?(indent = "") con =
  match con with
  | IntCon i -> printf "%s└──Int(%ld)\n" indent i
  | BoolCon b -> printf "%s└──Bool(%b)\n" indent b
  | UnitCon -> printf "%s└──Unit\n" indent
  | AdtCon (cname, tag) -> printf "%s└──%s : tag=%ld\n" indent cname tag

let rec pp_expr ?(indent = "") expr =
  let open Printf in
  let pp_node n = printf "%s└──%s\n" indent (string_of_expr_node n) in
  let pp_rec_expr = pp_expr ~indent:(indent ^ "   ") in
  match expr with
  | Int _ | Float _ | String _ | Bool _ | Ident _ | Unit | Constr _ ->
      pp_node expr
  | Bop (_, e0, _, e1) ->
      pp_node expr;
      pp_rec_expr e0;
      pp_rec_expr e1
  | Uop (_, _, e) ->
      pp_node expr;
      pp_rec_expr e
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
  | Tuple (_, es) | Direct_app (_, _, _, _, es) ->
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
  | ConstructorGet (_, _, e) ->
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
  | Shared_Expr ({ contents = shared }, _, _) ->
      pp_node expr;
      pp_rec_expr shared
  | While_true e | Break e ->
      pp_node expr;
      pp_rec_expr e
  | Assign_Seq assigns ->
      pp_node expr;
      let assign_indent = indent ^ "   " in
      List.iter
        (fun (x, ty, e) ->
          printf "%s└── assign %s : %s = \n" assign_indent x
            (Typed_ast.pp_texpr ty);
          pp_expr ~indent:(assign_indent ^ "   ") e)
        assigns
  | Hole -> pp_node expr
  | Set_Tuple (e0, e1, e2) ->
      pp_node expr;
      pp_rec_expr e0;
      pp_rec_expr e1;
      pp_rec_expr e2

and pp_case indent (pattern, expr) =
  printf "%s└── <case>\n" indent;
  let indent' = indent ^ "   " in
  pp_con ~indent:indent' pattern;
  pp_expr ~indent:indent' expr

let pp_tconstr ?(indent = "") =
  let open Printf in
  function
  | DeclConstr (cname, tag, None) ->
      printf "%s└──%s : tag=%ld\n" indent cname tag
  | DeclConstr (cname, tag, Some texpr) ->
      printf "%s└──%s of %s : tag=%ld\n" indent cname (Typed_ast.pp_texpr texpr)
        tag

let rec pp_decl ?(indent = "") =
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
  | And decls ->
      print_with_indent indent "And";
      List.iter (pp_decl ~indent:(indent ^ "   ")) decls
