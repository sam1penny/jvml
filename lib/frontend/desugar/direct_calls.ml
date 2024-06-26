(*
transform a call to a named function that passes all arguments into a direct call

e.g
let add = fun x -> fun y -> x + y
in
add 3 4

since both arguments are supplied to add, we can directly call its static method,
avoiding the need to create any closures.

required to spot tail recursion
*)
open Common
open Desugared_ast

let maybe_transform_direct env e =
  let rec maybe_transform_direct_inner nargs e =
    match e with
    | Ident (_, name) ->
        StringMap.find_opt name env >>= fun (ty_args, ty_ret) ->
        if nargs = List.length ty_args then Some (ty_args, ty_ret, name, [])
        else None
    | App (_, e0, e1) -> (
        match maybe_transform_direct_inner (nargs + 1) e0 with
        | None -> None
        | Some (ty_args, ty_ret, name, es) ->
            Some (ty_args, ty_ret, name, es @ [ e1 ]))
    | _ -> None
  in
  maybe_transform_direct_inner 0 e

let rec transform_direct_call_expr env e =
  let rec_transform_direct = transform_direct_call_expr env in
  match e with
  | Int _ | Float _ | String _ | Bool _ | Unit | Ident _ | Constr _
  | Match_Failure ->
      e
  | Bop (ty, e0, bop, e1) ->
      Bop (ty, rec_transform_direct e0, bop, rec_transform_direct e1)
  | Uop (ty, uop, e) -> Uop (ty, uop, rec_transform_direct e)
  | If (ty, e0, e1, e2) ->
      If
        ( ty,
          rec_transform_direct e0,
          rec_transform_direct e1,
          rec_transform_direct e2 )
  | Fun (t0, t1, x, e) -> Fun (t0, t1, x, rec_transform_direct e)
  | App (ret_ty, e0, e1) as e_app -> (
      match maybe_transform_direct env e_app with
      | None -> App (ret_ty, e0, rec_transform_direct e1)
      | Some (ty_args, fun_ty_ret, name, es) ->
          Direct_app
            (ret_ty, ty_args, fun_ty_ret, name, List.map rec_transform_direct es)
      )
  | Direct_app _ ->
      e
      (* already transformed before running trmc, no more transformations possible *)
  | Tuple (ty, es) -> Tuple (ty, List.map rec_transform_direct es)
  | Let (ty, x, e0, e1) ->
      let e0' = rec_transform_direct e0 in

      let env' =
        match Utils.collect_funargs e0 with
        | [], _ -> env
        | funargs, body ->
            let ty_args = List.map (fun (_, ty) -> ty) funargs in
            StringMap.add x (ty_args, get_expr_type body) env
      in
      let e1' = transform_direct_call_expr env' e1 in
      Let (ty, x, e0', e1')
  | LetRec (ty, x, e0, e1) ->
      let funargs, body = Utils.collect_funargs e0 in
      let ty_args = List.map (fun (_, ty) -> ty) funargs in
      let env' = StringMap.add x (ty_args, get_expr_type body) env in
      let e0' = transform_direct_call_expr env' e0 in
      let e1' = transform_direct_call_expr env' e1 in
      LetRec (ty, x, e0', e1')
  | Seq (ty, es) -> Seq (ty, List.map rec_transform_direct es)
  | TupleGet (tys, i, e) -> TupleGet (tys, i, rec_transform_direct e)
  | ConstructorGet (ty, cname, e) ->
      ConstructorGet (ty, cname, rec_transform_direct e)
  | Switch (ty, e0, cases, maybe_fallback_expr) ->
      Switch
        ( ty,
          rec_transform_direct e0,
          List.map
            (fun (con, case_expr) -> (con, rec_transform_direct case_expr))
            cases,
          Option.map rec_transform_direct maybe_fallback_expr )
  | Shared_Expr (expr_ref, maybe_label, seen) ->
      if !seen then e
      else (
        seen := true;
        let _ = expr_ref := rec_transform_direct !expr_ref in
        Shared_Expr (expr_ref, maybe_label, seen))
  | While_true _ | Break _ | Assign_Seq _ ->
      raise
      @@ Failure "tail rec constructs should not be present in direct_calls"
  | Hole | Set_Tuple _ -> raise @@ Failure "todo3"

let update_env env = function
  | Val (_, x, (Fun _ as e)) | ValRec (_, x, e) ->
      let funargs, body = Utils.collect_funargs e in
      let ty_args = List.map (fun (_, ty) -> ty) funargs in
      let env' = StringMap.add x (ty_args, get_expr_type body) env in
      env'
  | Val _ -> env
  | _ -> raise @@ Failure "illegal state"

let rec transform_direct_call_decl env decl =
  match decl with
  | Val (ty, x, (Fun _ as e)) ->
      let e' = transform_direct_call_expr env e in
      (Val (ty, x, e'), update_env env decl)
  | Val (ty, x, e) ->
      let e' = transform_direct_call_expr env e in
      (Val (ty, x, e'), env)
  | ValRec (ty, x, e) ->
      let env' = update_env env decl in
      let e' = transform_direct_call_expr env' e in
      (ValRec (ty, x, e'), env')
  | Type _ -> (decl, StringMap.empty)
  | And decls ->
      let env = List.fold_left update_env env decls in
      List.fold_left
        (fun (transformed_decls, env) decl ->
          let transformed_decl, env' = transform_direct_call_decl env decl in
          (transformed_decls @ [ transformed_decl ], env'))
        ([], env) decls
      |> fun (transformed_decls, env) -> (And transformed_decls, env)

let transform_direct_call_program program =
  let transformed_program =
    List.fold_left
      (fun (transformed_program, env) decl ->
        let transformed_decl, env' = transform_direct_call_decl env decl in
        (transformed_program @ [ transformed_decl ], env'))
      ([], StringMap.empty) program
    |> fun (transformed_program, _) -> transformed_program
  in
  Utils.clear_shared_program_seen transformed_program;
  transformed_program
