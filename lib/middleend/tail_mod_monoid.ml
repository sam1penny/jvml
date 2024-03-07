(*
automatic transform of functions that are
tail recursive modulo some number of monoid operators.

for example,
let rec length = fun x -> match x with
  | N -> 0
  | C(_, xs) -> 1 + length xs
*)
open Desugar.Desugared_ast
open Typing

(*
need a function for mapping onto tail positions.

rewrite as a separate function

if direct call, pass acc in
else pass 0
*)

let rec map_over_tail_positions f e =
  match e with
  | Int _ | Ident _ | Bool _ | Unit | Constr _ | Match_Failure | Fun _ | App _
  | Tuple _ | TupleGet _ | ConstructorGet _ | Bop _ | Direct_app _ ->
      f e
  | If (ty, e0, e1, e2) ->
      If (ty, e0, map_over_tail_positions f e1, map_over_tail_positions f e2)
  | Let (ty, x, e0, e1) -> Let (ty, x, e0, map_over_tail_positions f e1)
  | LetRec (ty, x, e0, e1) -> LetRec (ty, x, e0, map_over_tail_positions f e1)
  | Seq (ty, es) ->
      let last, rev_rest =
        let reversed = List.rev es in
        (List.hd reversed, List.tl reversed)
      in
      Seq (ty, List.rev (f last :: rev_rest))
  | Switch (ty, e, cases, maybe_fallback_expr) ->
      let cases' =
        List.map
          (fun (con, case_expr) -> (con, map_over_tail_positions f case_expr))
          cases
      in
      let maybe_fallback_expr' =
        Option.map (map_over_tail_positions f) maybe_fallback_expr
      in
      Switch (ty, e, cases', maybe_fallback_expr')
  (* unsafe if shared, need to add option field *)
  | Shared_Expr (e_ref, _) ->
      e_ref := map_over_tail_positions f !e_ref;
      e
  | While_true _ | Return _ | Assign_Seq _ ->
      raise
      @@ Failure
           "tail rec constructs should not be present before calling \
            tail_call_optimise"

let rec rewrite_apps fn_name e =
  match e with
  | Ident (ty, name) ->
      if name = fn_name then
        App
          ( ty,
            Ident (Typed_ast.TyFun (Typed_ast.TyInt, ty), name ^ "_acc"),
            Int 0 )
      else e
  | Direct_app (ret_ty, arg_tys, fun_ret_ty, name, arg_es) ->
      Direct_app
        ( ret_ty,
          [ Typed_ast.TyInt ] @ arg_tys,
          fun_ret_ty,
          name ^ "_acc",
          [ Ident (Typed_ast.TyInt, "acc") ] @ arg_es )
  | _ -> Desugar.Utils.map_over_sub_expr (rewrite_apps fn_name) e

(*
if the tail context is a tail call, simply thread in the accumulator

otherwise add the bop to the tail: acc + tail
*)
let thread_in_accumulator fn_name e =
  let foo e =
    match e with
    | Direct_app (_, _, _, name, _) when name = fn_name ^ "_acc" -> e
    | _ -> Bop (Typed_ast.TyInt, Ident (Typed_ast.TyInt, "acc"), ADD, e)
  in
  map_over_tail_positions foo e

let rec transform_tmm_expr fn_name e =
  let rec_transform_tc = transform_tmm_expr fn_name in
  match e with
  | Int _ | Ident _ | Bool _ | Unit | Constr _ | Match_Failure | Fun _ | App _
  | Tuple _ | TupleGet _ | ConstructorGet _ | Direct_app _ ->
      (e, false)
  | Bop (_, e0, ADD, Direct_app (ret_ty, arg_tys, fun_ret_ty, name, arg_es)) ->
      if name = fn_name then
        (* todo - dollar sign after acc *)
        ( Direct_app
            ( ret_ty,
              [ Typed_ast.TyInt ] @ arg_tys,
              fun_ret_ty,
              name ^ "_acc",
              [ rewrite_apps fn_name e0 |> thread_in_accumulator fn_name ]
              @ arg_es ),
          true )
      else (e, false)
  | If (ty, e0, e1, e2) ->
      let transformed_e1, did_tmm_e1 = rec_transform_tc e1 in
      let transformed_e2, did_tmm_e2 = rec_transform_tc e2 in
      if did_tmm_e1 || did_tmm_e2 then
        let e1' =
          if did_tmm_e1 then transformed_e1
          else
            rewrite_apps fn_name transformed_e1 |> thread_in_accumulator fn_name
        in
        let e2' =
          if did_tmm_e2 then transformed_e2
          else
            rewrite_apps fn_name transformed_e2 |> thread_in_accumulator fn_name
        in
        (If (ty, rewrite_apps fn_name e0, e1', e2'), true)
      else (e, false)
  | _ -> (e, false)

let transform_tmm_decl decl =
  match decl with
  | ValRec (ty, x, e) -> (
      let funargs, body = Desugar.Utils.collect_funargs e in
      let transformed_body, did_tmm = transform_tmm_expr x body in
      match did_tmm with
      | false ->
          [
            ValRec
              (ty, x, Desugar.Utils.replace_funargs funargs transformed_body);
          ]
      | true ->
          let expr_without_acc =
            Desugar.Utils.replace_funargs funargs transformed_body
          in
          let transformed_expr =
            Fun
              ( Typed_ast.TyInt,
                get_expr_type expr_without_acc,
                "acc",
                expr_without_acc )
          in

          let original_body =
            Direct_app
              ( get_expr_type body,
                [ Typed_ast.TyInt ] @ List.map (fun (_, ty) -> ty) funargs,
                get_expr_type body,
                x ^ "_acc",
                [ Int 0 ] @ List.map (fun (arg, ty) -> Ident (ty, arg)) funargs
              )
          in
          let original_expr =
            Desugar.Utils.replace_funargs funargs original_body
          in
          [
            ValRec (ty, x ^ "_acc", transformed_expr);
            ValRec (ty, x, original_expr);
          ])
  | _ -> [ decl ]

let transform_tmm_program program =
  List.map transform_tmm_decl program |> List.flatten
