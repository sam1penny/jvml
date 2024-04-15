open Desugar.Desugared_ast
open Typing

let dummy_ty = Typed_ast.TyTuple []

let rec has_tmc_candidate fn_name e =
  let rec_has_tmc = has_tmc_candidate fn_name in
  match e with
  | Int _ | Ident _ | Float _ | String _ | Bool _ | Unit | Constr _
  | Match_Failure ->
      false
  | Bop _ | Fun _ | TupleGet _ | ConstructorGet _ | Direct_app _ | Tuple _ ->
      false
  | If (_, _, e1, e2) -> rec_has_tmc e1 || rec_has_tmc e2
  | App (_, Constr (_, _), Tuple (_, es)) ->
      List.exists
        (function
          | Direct_app (_, _, _, name, _) when name = fn_name -> true
          | _ -> false)
        es
  | App _ -> false
  | Let (_, _, _, e1) -> rec_has_tmc e1
  | LetRec (_, _, _, e1) -> rec_has_tmc e1
  | Seq (_, es) ->
      let last = List.rev es |> List.hd in
      rec_has_tmc last
  | Switch (_, _, cases, maybe_fallback_expr) ->
      let case_tmc =
        List.map (fun (_, case_expr) -> rec_has_tmc case_expr) cases
      in
      let fallback_tmc =
        Option.map rec_has_tmc maybe_fallback_expr
        |> Option.value ~default:false
      in
      List.exists (fun b -> b) case_tmc |> ( || ) fallback_tmc
  | Shared_Expr (e_ref, _, _) -> rec_has_tmc !e_ref
  | While_true _ | Break _ | Assign_Seq _ | Hole | Set_Tuple _ ->
      raise @@ Failure "tail rec constructs should not be present yet"

let has_tmc_decl decl =
  match decl with
  | ValRec (_, fname, e) ->
      let _, body = Desugar.Utils.collect_funargs e in
      has_tmc_candidate fname body
  | _ -> false

let replace_first_direct_with_hole fn_name es =
  let rec inner i es =
    match es with
    | [] -> (None, [])
    | Direct_app (ret_ty, args_ty, fun_ret_ty, name, args) :: es
      when name = fn_name ->
        (Some (i, (ret_ty, args_ty, fun_ret_ty, name, args)), Hole :: es)
    | e :: es ->
        let maybe_dapp, es = inner (i + 1) es in
        (maybe_dapp, e :: es)
  in
  inner 0 es

let replace_old_return_ty_with_unit ty =
  let rec replace_inner = function
    | Typed_ast.TyFun (t0, t1) -> Typed_ast.TyFun (t0, replace_inner t1)
    | _ -> Typed_ast.TyUnit
  in
  replace_inner ty

let rec replace_call_with_dps fn_name dst_ty e =
  match e with
  | Ident (old_ty, name) when name = fn_name ->
      let new_ty =
        Typed_ast.TyFun
          ( dst_ty,
            Typed_ast.TyFun
              (Typed_ast.TyInt, replace_old_return_ty_with_unit old_ty) )
      in
      App
        ( old_ty,
          App
            ( Typed_ast.TyFun
                (Typed_ast.TyInt, replace_old_return_ty_with_unit old_ty),
              Ident (new_ty, name ^ "_dps"),
              Ident (dst_ty, "dst") ),
          Ident (Typed_ast.TyInt, "i") )
  | Direct_app (_, args_ty, _, name, arg_es) when name = fn_name ->
      Direct_app
        ( Typed_ast.TyUnit,
          [ dst_ty; Typed_ast.TyInt ] @ args_ty,
          Typed_ast.TyUnit,
          name ^ "_dps",
          [ Ident (dst_ty, "dst"); Ident (Typed_ast.TyInt, "i") ] @ arg_es )
  | e ->
      Desugar.Utils.map_over_sub_expr (replace_call_with_dps fn_name dst_ty) e

let rec transform_tmc_dps_expr fn_name e =
  match e with
  | App (ty, Constr (constr_ty, cname), Tuple (tuple_ty, es)) -> (
      let maybe_dapp, es_with_hole =
        replace_first_direct_with_hole fn_name es
      in
      match maybe_dapp with
      | None -> e
      | Some (i, (_, args_ty, _, name, args)) ->
          (* add dst tuple and i argument *)
          let dps_direct_app =
            Direct_app
              ( Typed_ast.TyUnit,
                [ tuple_ty; Typed_ast.TyInt ] @ args_ty,
                Typed_ast.TyUnit,
                name ^ "_dps",
                [ Ident (tuple_ty, "dst'"); Int (Int32.of_int i) ] @ args )
          in
          Let
            ( tuple_ty,
              "dst'",
              Tuple (tuple_ty, es_with_hole),
              Seq
                ( get_expr_type dps_direct_app,
                  [
                    Set_Tuple
                      ( Ident (Typed_ast.TyInt, "i"),
                        Ident (tuple_ty, "dst"),
                        App
                          ( ty,
                            Constr (constr_ty, cname),
                            Ident (tuple_ty, "dst'") ) );
                    dps_direct_app;
                  ] ) ))
  (* write result*)
  | Int _ | Float _ | String _ | Ident _ | Bool _ | Unit | Constr _ | Fun _
  | App _ | Tuple _ | TupleGet _ | ConstructorGet _ | Bop _ | Direct_app _
  | Hole | Set_Tuple _ ->
      Set_Tuple
        ( Ident (Typed_ast.TyInt, "i"),
          (* choose random type, as I don't believe it is used *)
          Ident (dummy_ty, "dst"),
          e )
  | Match_Failure -> Match_Failure
  | _ ->
      Desugar.Utils.shallow_map_tail_positions
        (transform_tmc_dps_expr fn_name)
        e

let rec transform_tmc_nodps_expr fn_name e =
  match e with
  | App (ty, Constr (constr_ty, cname), Tuple (tuple_ty, es)) -> (
      let maybe_dapp, es_with_hole =
        replace_first_direct_with_hole fn_name es
      in
      match maybe_dapp with
      | None -> e
      | Some (i, (_, args_ty, _, name, args)) ->
          let dps_direct_app =
            Direct_app
              ( Typed_ast.TyUnit,
                [ tuple_ty; Typed_ast.TyInt ] @ args_ty,
                Typed_ast.TyUnit,
                name ^ "_dps",
                [ Ident (tuple_ty, "dst"); Int (Int32.of_int i) ] @ args )
          in
          Let
            ( tuple_ty,
              "dst",
              Tuple (tuple_ty, es_with_hole),
              Seq
                ( ty,
                  [
                    dps_direct_app;
                    App (ty, Constr (constr_ty, cname), Ident (tuple_ty, "dst"));
                  ] ) ))
  | _ ->
      Desugar.Utils.shallow_map_tail_positions
        (transform_tmc_nodps_expr fn_name)
        e

(*
Bodge to replace dps body type with TyUnit. Brittle.

Ideally, we'd run type inference again
*)
let rec replace_expr_ty new_ty e =
  match e with
  | Int _ | Float _ | String _ | Bool _ | Unit | Match_Failure | Hole
  | Set_Tuple _ | Assign_Seq _ ->
      e
  | Ident (_, i) -> Ident (new_ty, i)
  | Bop (_, e0, bop, e1) -> Bop (new_ty, e0, bop, e1)
  | If (_, e0, e1, e2) -> If (new_ty, e0, e1, e2)
  | App (_, e0, e1) -> App (new_ty, e0, e1)
  | Direct_app (_, arg_tys, ret_ty, name, arg_es) ->
      Direct_app (new_ty, arg_tys, ret_ty, name, arg_es)
  | Switch (_, e, cases, maybe_fallback_expr) ->
      Switch (new_ty, e, cases, maybe_fallback_expr)
  | Let (_, x, e0, e1) -> Let (new_ty, x, e0, e1)
  | LetRec (_, x, e0, e1) -> LetRec (new_ty, x, e0, e1)
  | Shared_Expr (e_ref, _, _) ->
      e_ref := replace_expr_ty new_ty !e_ref;
      e
  | While_true e -> While_true (replace_expr_ty new_ty e)
  | Break e -> Break (replace_expr_ty new_ty e)
  | Seq (_, es) -> Seq (new_ty, es)
  (* todo other cases *)
  | _ -> e

(* todo extend to 'and' *)
let transform_tmc_decl decl =
  let open Typing in
  match decl with
  | ValRec (ty, fname, e) -> (
      match has_tmc_decl decl with
      | false -> [ decl ]
      | true ->
          let funargs, body = Desugar.Utils.collect_funargs e in
          let dps_body =
            transform_tmc_dps_expr fname
              (Desugar.Utils.duplicate_shared_exprs_once body)
          in
          Desugar.Utils.clear_shared_expr_seen dps_body;

          let dps_body = replace_call_with_dps fname dummy_ty dps_body in
          Desugar.Utils.clear_shared_expr_seen dps_body;
          (* temporarily wack in random type, hope it isn't checked *)
          let dps_ty =
            Typed_ast.TyFun
              ( dummy_ty,
                TyFun (Typed_ast.TyInt, replace_old_return_ty_with_unit ty) )
          in

          let dps_fun =
            Fun
              ( dummy_ty,
                Typed_ast.TyFun
                  (Typed_ast.TyInt, replace_old_return_ty_with_unit ty),
                "dst",
                Fun
                  ( Typed_ast.TyInt,
                    replace_old_return_ty_with_unit ty,
                    "i",
                    Desugar.Utils.replace_funargs funargs
                      (replace_expr_ty TyUnit dps_body) ) )
          in

          let no_dps_body = transform_tmc_nodps_expr fname body in
          Desugar.Utils.clear_shared_expr_seen no_dps_body;
          let no_dps_fun = Desugar.Utils.replace_funargs funargs no_dps_body in
          [
            ValRec (dps_ty, fname ^ "_dps", dps_fun);
            ValRec (ty, fname, no_dps_fun);
          ])
  | _ -> [ decl ]

let transform_tmc_program program =
  List.map transform_tmc_decl program |> List.flatten
