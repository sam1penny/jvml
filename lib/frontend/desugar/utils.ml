open Desugared_ast

let rec collect_funargs = function
  | Fun (t0, _, x, e) ->
      let funargs, e = collect_funargs e in
      ((x, t0) :: funargs, e)
  | e -> ([], e)

let replace_funargs funargs body =
  List.fold_right
    (fun (arg, arg_ty) b -> Fun (arg_ty, get_expr_type b, arg, b))
    funargs body

let rec map_over_decl_exprs f d =
  match d with
  | Val (ty, x, e) -> Val (ty, x, f e)
  | ValRec (ty, x, e) -> ValRec (ty, x, f e)
  | Type _ -> d
  | And decls -> And (List.map (map_over_decl_exprs f) decls)

let map_over_sub_expr f e =
  match e with
  | Int _ | Ident _ | Bool _ | Unit | Constr _ | Match_Failure -> e
  | Bop (ty, e0, bop, e1) ->
      let e0' = f e0 in
      let e1' = f e1 in
      Bop (ty, e0', bop, e1')
  | If (ty, e0, e1, e2) ->
      let e0' = f e0 in
      let e1' = f e1 in
      let e2' = f e2 in
      If (ty, e0', e1', e2')
  | Fun (t0, t1, x, e) -> Fun (t0, t1, x, f e)
  | App (ty, e0, e1) ->
      let e0' = f e0 in
      let e1' = f e1 in
      App (ty, e0', e1')
  | Direct_app (ret_ty, args_ty, fun_ret_ty, name, es) ->
      Direct_app (ret_ty, args_ty, fun_ret_ty, name, List.map f es)
  | Tuple (ty, es) -> Tuple (ty, List.map f es)
  | Let (ty, x, e0, e1) ->
      let e0' = f e0 in
      let e1' = f e1 in
      Let (ty, x, e0', e1')
  | LetRec (ty, x, e0, e1) ->
      let e0' = f e0 in
      let e1' = f e1 in
      LetRec (ty, x, e0', e1')
  | Seq (ty, es) -> Seq (ty, List.map f es)
  | TupleGet (ty, i, e) -> TupleGet (ty, i, f e)
  | ConstructorGet (ty, c, e) -> ConstructorGet (ty, c, f e)
  | Switch (ty, e0, cases, maybe_fallback_expr) ->
      let e0' = f e0 in
      let cases' =
        List.map (fun (con, case_expr) -> (con, f case_expr)) cases
      in
      Switch (ty, e0', cases', Option.map f maybe_fallback_expr)
  | Shared_Expr (expr_ref, label_opt, seen) as shared_expr ->
      if !seen then shared_expr
      else (
        seen := true;
        expr_ref := f !expr_ref;
        Shared_Expr (expr_ref, label_opt, seen))
  | While_true e -> While_true (f e)
  | Return e -> Return (f e)
  | Assign_Seq assigns ->
      Assign_Seq (List.map (fun (x, ty, e) -> (x, ty, f e)) assigns)

let iter_over_sub_expr f e =
  let _ =
    map_over_sub_expr
      (fun x ->
        f x;
        x)
      e
  in
  ()

let iter_over_decl_exprs f d =
  let _ =
    map_over_decl_exprs
      (fun x ->
        f x;
        x)
      d
  in
  ()

let rec clear_shared_expr_seen e =
  match e with
  | Shared_Expr (_, _, seen) -> seen := false
  | _ -> iter_over_sub_expr clear_shared_expr_seen e

let clear_shared_decl_seen d = iter_over_decl_exprs clear_shared_expr_seen d
let clear_shared_program_seen program = List.iter clear_shared_decl_seen program
