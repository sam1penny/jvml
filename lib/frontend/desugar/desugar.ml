open Typing
open Desugared_ast

let rec desugar_expr constructors_by_type expr =
  let rec_desugar = desugar_expr constructors_by_type in
  match expr with
  | Typed_ast.Int (_, i) -> Int i
  | Typed_ast.Ident (_, ty, x) -> Ident (ty, x)
  | Typed_ast.Bool (_, b) -> Bool b
  | Typed_ast.Unit _ -> Unit
  | Typed_ast.Bop (_, ty, e0, op, e1) ->
      Bop (ty, rec_desugar e0, op, rec_desugar e1)
  | Typed_ast.If (_, ty, e0, e1, e2) ->
      If (ty, rec_desugar e0, rec_desugar e1, rec_desugar e2)
  | Typed_ast.Fun (_, t0, t1, x, e) -> Fun (t0, t1, x, rec_desugar e)
  | Typed_ast.App (_, ty, e0, e1) -> App (ty, rec_desugar e0, rec_desugar e1)
  | Typed_ast.Match (_, _, _, cases) ->
      let desugared_cases =
        List.map (fun (pat, expr) -> (pat, rec_desugar expr)) cases
      in
      let clauses = List.map Compile_patterns.clause_of_case desugared_cases in
      Compile_patterns.compile_match constructors_by_type clauses
  | Typed_ast.Tuple (_, ty, es) -> Tuple (ty, List.map rec_desugar es)
  | Typed_ast.Let (_, ty, x, e0, e1) ->
      Let (ty, x, rec_desugar e0, rec_desugar e1)
  | Typed_ast.LetRec (_, ty, x, e0, e1) ->
      LetRec (ty, x, rec_desugar e0, rec_desugar e1)
  | Typed_ast.Constr (_, ty, cname) -> Constr (ty, cname)
  | Typed_ast.Seq (_, ty, es) -> Seq (ty, List.map rec_desugar es)

let desugar_type_constructor = function
  | Typed_ast.DeclConstr (_, cname, ty_opt) -> DeclConstr (cname, ty_opt)

let desugar_decl constructors_by_type = function
  | Typed_ast.Val (_, ty, x, e) ->
      (Val (ty, x, desugar_expr constructors_by_type e), constructors_by_type)
  | Typed_ast.ValRec (_, ty, x, e) ->
      (ValRec (ty, x, desugar_expr constructors_by_type e), constructors_by_type)
  | Typed_ast.Type (_, ty, params, tname, constructors) ->
      let desugared_constructors =
        List.map desugar_type_constructor constructors
      in
      let testcons =
        List.map
          (function DeclConstr (cname, _) -> AdtCon cname)
          desugared_constructors
      in
      ( Type (ty, params, tname, List.map desugar_type_constructor constructors),
        (tname, testcons) :: constructors_by_type )

let desugar_program program =
  List.fold_left
    (fun (constructors_by_type, desugared_program_rev) decl ->
      let desugared_decl, constructors_by_type =
        desugar_decl constructors_by_type decl
      in
      (constructors_by_type, desugared_decl :: desugared_program_rev))
    ([], []) program
  |> fun (_, reversed_program) -> List.rev reversed_program

module Desugared_ast = Desugared_ast
module Compile_patterns = Compile_patterns
