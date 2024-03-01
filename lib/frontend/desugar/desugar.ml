open Typing
open Desugared_ast
open Common

(* todo - tidy up by encapsulating this into an argument *)
let desugared_temp_var =
  let n = ref 0 in
  fun () ->
    let x = !n in
    n := x + 1;
    "desugar_t" ^ string_of_int x

let rec desugared_ast_of_expr constructors_by_type expr =
  let rec_desugar = desugared_ast_of_expr constructors_by_type in
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
  | Typed_ast.Match (_, ty, e, cases) ->
      let desugared_cases =
        List.map (fun (pat, expr) -> (pat, rec_desugar expr)) cases
      in
      let var = desugared_temp_var () in
      let clauses =
        List.map
          (Compile_patterns.clause_of_case (Ident (Infer.get_expr_type e, var)))
          desugared_cases
      in
      let table = Hashtbl.create 10 in
      Let
        ( ty,
          var,
          rec_desugar e,
          Compile_patterns.compile_match table constructors_by_type ty clauses
        )
  | Typed_ast.Tuple (_, ty, es) -> Tuple (ty, List.map rec_desugar es)
  | Typed_ast.Let (_, ty, x, e0, e1) ->
      Let (ty, x, rec_desugar e0, rec_desugar e1)
  | Typed_ast.LetRec (_, ty, x, e0, e1) ->
      LetRec (ty, x, rec_desugar e0, rec_desugar e1)
  | Typed_ast.Constr (_, ty, cname) -> Constr (ty, cname)
  | Typed_ast.Seq (_, ty, es) -> Seq (ty, List.map rec_desugar es)

let desugared_ast_of_type_constructor tag = function
  | Typed_ast.DeclConstr (_, cname, ty_opt) -> DeclConstr (cname, tag, ty_opt)

let desugared_ast_of_decl constructors_by_type = function
  | Typed_ast.Val (_, ty, x, e) ->
      ( constructors_by_type,
        Val (ty, x, desugared_ast_of_expr constructors_by_type e) )
  | Typed_ast.ValRec (_, ty, x, e) ->
      ( constructors_by_type,
        ValRec (ty, x, desugared_ast_of_expr constructors_by_type e) )
  | Typed_ast.Type (_, ty, params, tname, constructors) ->
      let desugared_constructors =
        List.mapi desugared_ast_of_type_constructor constructors
      in
      let casecons_by_name =
        List.fold_left
          (fun acc (DeclConstr (cname, tag, _)) ->
            StringMap.add cname (AdtCon (cname, tag)) acc)
          StringMap.empty desugared_constructors
      in
      ( StringMap.add tname casecons_by_name constructors_by_type,
        Type (ty, params, tname, desugared_constructors) )

(*
- compile patterns and desugar constructors
- necessary desugaring to use desugared_ast
*)
let desugared_ast_of_program program =
  List.fold_left
    (fun (constructors_by_type, desugared_program_rev) decl ->
      let constructors_by_type, desugared_decl =
        desugared_ast_of_decl constructors_by_type decl
      in
      (constructors_by_type, desugared_decl :: desugared_program_rev))
    (StringMap.empty, []) program
  |> fun (_, reversed_program) -> List.rev reversed_program

let desugar_program program =
  desugared_ast_of_program program
  |> Unique_names.rename_program
  |> Constant_fold.constant_fold_program
  |> Lambda_lift.lift_lambdas_program
  |> Direct_calls.transform_direct_call_program

module Desugared_ast = Desugared_ast
module Compile_patterns = Compile_patterns
module Unique_names = Unique_names
module Lambda_lift = Lambda_lift
module Tail_call_optimise = Tail_call_optimise
module Constant_fold = Constant_fold
