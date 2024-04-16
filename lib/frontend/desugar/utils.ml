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
  | Int _ | Float _ | String _ | Ident _ | Bool _ | Unit | Constr _
  | Match_Failure ->
      e
  | Bop (ty, e0, bop, e1) ->
      let e0' = f e0 in
      let e1' = f e1 in
      Bop (ty, e0', bop, e1')
  | Uop (ty, uop, e) ->
      let e' = f e in
      Uop (ty, uop, e')
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
  | Break e -> Break (f e)
  | Assign_Seq assigns ->
      Assign_Seq (List.map (fun (x, ty, e) -> (x, ty, f e)) assigns)
  | Hole -> Hole
  | Set_Tuple (e0, e1, e2) ->
      let e0' = f e0 in
      let e1' = f e1 in
      let e2' = f e2 in
      Set_Tuple (e0', e1', e2')

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

let fold_left_over_sub_expr f acc e =
  match e with
  | Int _ | Float _ | String _ | Ident _ | Bool _ | Unit | Constr _
  | Match_Failure | Hole ->
      acc
  | Bop (_, e0, _, e1) -> f (f acc e0) e1
  | Uop (_, _, e) -> f acc e
  | If (_, e0, e1, e2) -> f (f (f acc e0) e1) e2
  | Fun (_, _, _, e) -> f acc e
  | App (_, e0, e1) -> f (f acc e0) e1
  | Direct_app (_, _, _, _, arg_es) -> List.fold_left f acc arg_es
  | Tuple (_, es) | Seq (_, es) -> List.fold_left f acc es
  | Let (_, _, e0, e1) | LetRec (_, _, e0, e1) -> f (f acc e0) e1
  | TupleGet (_, _, e) | ConstructorGet (_, _, e) -> f acc e
  | Switch (_, e, cases, maybe_fallback_expr) -> (
      List.map (fun (_, case_expr) -> case_expr) cases
      |> List.fold_left f (f acc e)
      |> fun acc' ->
      match maybe_fallback_expr with
      | None -> acc'
      | Some fallback_expr -> f acc' fallback_expr)
  | Shared_Expr (e_ref, _, seen) ->
      if !seen then acc
      else (
        seen := true;
        f acc !e_ref)
  | While_true e | Break e -> f acc e
  | Assign_Seq assigns ->
      List.map (fun (_, _, e) -> e) assigns |> List.fold_left f acc
  | Set_Tuple (e0, e1, e2) -> f (f (f acc e0) e1) e2

let rec clear_shared_expr_seen e =
  match e with
  | Shared_Expr (e_ref, _, seen) ->
      seen := false;
      clear_shared_expr_seen !e_ref
  | _ -> iter_over_sub_expr clear_shared_expr_seen e

let clear_shared_decl_seen d = iter_over_decl_exprs clear_shared_expr_seen d
let clear_shared_program_seen program = List.iter clear_shared_decl_seen program

let rec map_over_expr_texprs f expr =
  match expr with
  | Int _ | Float _ | String _ | Bool _ | Unit | Match_Failure -> expr
  | Ident (ty, x) -> Ident (f ty, x)
  | Bop (ty, e0, op, e1) ->
      let ty' = f ty in
      let e0' = map_over_expr_texprs f e0 in
      let e1' = map_over_expr_texprs f e1 in
      Bop (ty', e0', op, e1')
  | Uop (ty, uop, e) ->
      let ty' = f ty in
      let e' = map_over_expr_texprs f e in
      Uop (ty', uop, e')
  | If (ty, e0, e1, e2) ->
      let ty' = f ty in
      let e0' = map_over_expr_texprs f e0 in
      let e1' = map_over_expr_texprs f e1 in
      let e2' = map_over_expr_texprs f e2 in
      If (ty', e0', e1', e2')
  | Fun (t0, t1, x, e) ->
      let t0' = f t0 in
      let t1' = f t1 in
      Fun (t0', t1', x, map_over_expr_texprs f e)
  | App (ty, e0, e1) ->
      let ty' = f ty in
      let e0' = map_over_expr_texprs f e0 in
      let e1' = map_over_expr_texprs f e1 in
      App (ty', e0', e1')
  | Direct_app (ty, arg_tys, ret_ty, name, arg_es) ->
      let ty' = f ty in
      let arg_tys' = List.map f arg_tys in
      let ret_ty' = f ret_ty in
      let arg_es' = List.map (map_over_expr_texprs f) arg_es in
      Direct_app (ty', arg_tys', ret_ty', name, arg_es')
  | Tuple (ty, es) ->
      let ty' = f ty in
      Tuple (ty', List.map (map_over_expr_texprs f) es)
  | Switch (ty, e, cases, maybe_fallback_expr) ->
      let ty' = f ty in
      let e' = map_over_expr_texprs f e in
      let cases' =
        List.map
          (fun (case_con, case_expr) ->
            (case_con, map_over_expr_texprs f case_expr))
          cases
      in
      let maybe_fallback_expr' =
        Option.map (map_over_expr_texprs f) maybe_fallback_expr
      in
      Switch (ty', e', cases', maybe_fallback_expr')
  | Let (ty, x, e0, e1) ->
      let ty' = f ty in
      let e0' = map_over_expr_texprs f e0 in
      let e1' = map_over_expr_texprs f e1 in
      Let (ty', x, e0', e1')
  | LetRec (ty, x, e0, e1) ->
      let ty' = f ty in
      let e0' = map_over_expr_texprs f e0 in
      let e1' = map_over_expr_texprs f e1 in
      LetRec (ty', x, e0', e1')
  | Constr (ty, cname) -> Constr (f ty, cname)
  | Seq (ty, es) ->
      let ty' = f ty in
      Seq (ty', List.map (map_over_expr_texprs f) es)
  | ConstructorGet (ty, cname, e) ->
      let ty' = f ty in
      ConstructorGet (ty', cname, map_over_expr_texprs f e)
  | TupleGet (ty, i, e) ->
      let ty' = f ty in
      TupleGet (ty', i, map_over_expr_texprs f e)
  | Shared_Expr (e_ref, _, _) ->
      let _ = map_over_expr_texprs f !e_ref in
      expr
  | While_true e -> While_true (map_over_expr_texprs f e)
  | Break e -> Break (map_over_expr_texprs f e)
  | Assign_Seq assigns ->
      Assign_Seq
        (List.map
           (fun (x, ty, e) -> (x, f ty, map_over_expr_texprs f e))
           assigns)
  | Hole -> Hole
  | Set_Tuple (e0, e1, e2) ->
      let e0' = map_over_expr_texprs f e0 in
      let e1' = map_over_expr_texprs f e1 in
      let e2' = map_over_expr_texprs f e2 in
      Set_Tuple (e0', e1', e2')

let shallow_map_tail_positions f e =
  match e with
  | Int _ | Float _ | String _ | Ident _ | Bool _ | Unit | Constr _
  | Match_Failure | Fun _ | App _ | Tuple _ | TupleGet _ | ConstructorGet _
  | Bop _ | Uop _ | Direct_app _ | Hole | Set_Tuple _ ->
      e
  | If (ty, e0, e1, e2) -> If (ty, e0, f e1, f e2)
  | Let (ty, x, e0, e1) -> Let (ty, x, e0, f e1)
  | LetRec (ty, x, e0, e1) -> LetRec (ty, x, e0, f e1)
  | Seq (ty, es) ->
      let last, rev_rest =
        let reversed = List.rev es in
        (List.hd reversed, List.tl reversed)
      in
      Seq (ty, List.rev (f last :: rev_rest))
  | Switch (ty, e, cases, maybe_fallback_expr) ->
      let cases' =
        List.map (fun (con, case_expr) -> (con, f case_expr)) cases
      in
      let maybe_fallback_expr' = Option.map f maybe_fallback_expr in
      Switch (ty, e, cases', maybe_fallback_expr')
  (* unsafe if shared, need to add option field *)
  | Shared_Expr (e_ref, _, seen) ->
      if !seen then e
      else (
        seen := true;
        e_ref := f !e_ref;
        e)
  | While_true _ | Break _ | Assign_Seq _ ->
      raise @@ Failure "tail rec constructs should not be present yet!"

let duplicate_shared_exprs_once e =
  let shared_expr_tbl = Hashtbl.create 10 in
  let rec duplicate_shared_exprs_once_inner e =
    match e with
    | Shared_Expr (e_ref, _, _) -> (
        match Hashtbl.find_opt shared_expr_tbl e_ref with
        | None ->
            let new_e = duplicate_shared_exprs_once_inner !e_ref in
            let copy = Shared_Expr (ref new_e, ref None, ref false) in
            Hashtbl.add shared_expr_tbl e_ref copy;
            copy
        | Some shared_expr -> shared_expr)
    | _ -> map_over_sub_expr duplicate_shared_exprs_once_inner e
  in
  duplicate_shared_exprs_once_inner e
