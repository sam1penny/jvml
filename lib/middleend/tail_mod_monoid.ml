(*
automatic transform of functions that are
tail recursive modulo some number of commutative monoid operators.

for example,
let rec length = fun x -> match x with
  | N -> 0
  | C(_, xs) -> 1 + length xs
*)
open Desugar.Desugared_ast
open Typing
open Common

let or_else f k o = match o with None -> f k | Some _ -> o

let rec has_tmm_expr fn_name e =
  let rec_has_tmm = has_tmm_expr fn_name in
  match e with
  | Int _ | Float _ | String _ | Ident _ | Bool _ | Unit | Constr _
  | Match_Failure | Fun _ | App _ | Tuple _ | TupleGet _ | ConstructorGet _
  | Direct_app _ | Hole | Set_Tuple _ | Uop _ ->
      None
  | Bop (_, _, ((ADD | MUL) as bop), Direct_app (_, _, _, name, _))
    when name = fn_name ->
      Some bop
  | Bop (_, Direct_app (_, _, _, name, _), ((ADD | MUL) as bop), e1)
    when name = fn_name
         && Inline.poor_mans_effect_safety_with_exceptions
              (StringSet.singleton fn_name)
              e1 ->
      Some bop
  | Bop (_, e0, ((ADD | MUL) as bop), e1)
    when Inline.poor_mans_effect_safety_with_exceptions
           (StringSet.singleton fn_name)
           e1 ->
      rec_has_tmm e0
      >>= (fun b -> if b = bop then Some bop else None)
      |> or_else rec_has_tmm e1
      >>= fun b -> if b = bop then Some bop else None
  | Bop _ -> None
  | If (_, _, e1, e2) -> rec_has_tmm e1 |> or_else rec_has_tmm e2
  | Let (_, _, _, e1) | LetRec (_, _, _, e1) -> rec_has_tmm e1
  | Seq (_, es) ->
      let last = List.rev es |> List.hd in
      rec_has_tmm last
  | Switch (_, _, cases, maybe_fallback_expr) ->
      List.map (fun (_, case_expr) -> rec_has_tmm case_expr) cases
      |> List.find_opt Option.is_some
      |> Option.join
      |> or_else
           (fun e -> Option.map rec_has_tmm e |> Option.join)
           maybe_fallback_expr
  | Shared_Expr (e_ref, _, _) -> rec_has_tmm !e_ref
  | While_true _ | Break _ | Assign_Seq _ ->
      raise
      @@ Failure
           "tail rec constructs should not be present before calling \
            tail_call_optimise"

let get_identity_element = function
  | ADD -> Int 0l
  | MUL -> Int 1l
  | _ ->
      raise
      @@ Failure
           "called get_identity_element on operator that is not both \
            associative and commutative"

(*
rewrite applications not in the tail position,

or partial applications in tail position

to initialise the accumulator to identity element
(0 for addition, 1 for multiplication).
*)
let rec rewrite_apps modulo_bop fn_name e =
  match e with
  | Ident (ty, name) ->
      if name = fn_name then
        App
          ( ty,
            Ident (Typed_ast.TyFun (Typed_ast.TyInt, ty), name ^ "_acc$"),
            get_identity_element modulo_bop )
      else e
  | Direct_app (ret_ty, arg_tys, fun_ret_ty, name, arg_es) ->
      if name = fn_name then
        Direct_app
          ( ret_ty,
            [ Typed_ast.TyInt ] @ arg_tys,
            fun_ret_ty,
            name ^ "_acc$",
            [ get_identity_element modulo_bop ] @ arg_es )
      else e
  | _ -> Desugar.Utils.map_over_sub_expr (rewrite_apps modulo_bop fn_name) e

let rec pass_to_accumulator modulo_bop e0 e1 =
  match (e0, e1) with
  | Direct_app (ret_ty, arg_tys, fun_ret_ty, name, arg_es), e1 ->
      let acc_expr = List.hd arg_es in
      let acc_expr' = pass_to_accumulator modulo_bop acc_expr e1 in
      Direct_app (ret_ty, arg_tys, fun_ret_ty, name, acc_expr' :: List.tl arg_es)
  | e0, Direct_app (ret_ty, arg_tys, fun_ret_ty, name, arg_es) ->
      let acc_expr = List.hd arg_es in
      let acc_expr' = pass_to_accumulator modulo_bop acc_expr e0 in
      Direct_app (ret_ty, arg_tys, fun_ret_ty, name, acc_expr' :: List.tl arg_es)
  | _, e1 -> Bop (Typed_ast.TyInt, e0, modulo_bop, e1)

(*
if the tail context is a tail call, simply thread in the accumulator

otherwise add the bop to the tail: acc + tail
*)
let rec transform_tmm_expr modulo_bop fn_name e =
  let rec_transform_tmm = transform_tmm_expr modulo_bop fn_name in
  match e with
  | Direct_app (ret_ty, args_ty, fun_ret_ty, name, arg_es) when name = fn_name
    ->
      Direct_app
        ( ret_ty,
          [ Typed_ast.TyInt ] @ args_ty,
          fun_ret_ty,
          name ^ "_acc$",
          [ Ident (Typed_ast.TyInt, "acc$") ] @ arg_es )
  | Int _ | Ident _ | Bool _ | Unit | Constr _ | Match_Failure | Fun _ | App _
  | Tuple _ | TupleGet _ | ConstructorGet _ | Direct_app _ ->
      Bop (Typed_ast.TyInt, Ident (Typed_ast.TyInt, "acc$"), modulo_bop, e)
  | Bop
      ( _,
        e',
        ((ADD | MUL) as bop),
        Direct_app (ret_ty, arg_tys, fun_ret_ty, name, arg_es) )
    when bop = modulo_bop ->
      if name = fn_name then
        Direct_app
          ( ret_ty,
            [ Typed_ast.TyInt ] @ arg_tys,
            fun_ret_ty,
            name ^ "_acc$",
            [ rec_transform_tmm e' ] @ arg_es )
      else e
  | Bop
      ( _,
        Direct_app (ret_ty, arg_tys, fun_ret_ty, name, arg_es),
        ((ADD | MUL) as bop),
        e' )
    when bop = modulo_bop
         && Inline.poor_mans_effect_safety_with_exceptions
              (StringSet.singleton fn_name)
              e' ->
      if name = fn_name then
        Direct_app
          ( ret_ty,
            [ Typed_ast.TyInt ] @ arg_tys,
            fun_ret_ty,
            name ^ "_acc$",
            [ rec_transform_tmm e' ] @ arg_es )
      else e
  | Bop (_, e0, ((ADD | MUL) as bop), e1)
    when bop = modulo_bop
         && Inline.poor_mans_effect_safety_with_exceptions
              (StringSet.singleton fn_name)
              e1 -> (
      let transformed_e0 = rec_transform_tmm e0 in
      let transformed_e1 = rec_transform_tmm e1 in
      match (transformed_e0, transformed_e1) with
      | Direct_app _, Direct_app _ ->
          pass_to_accumulator modulo_bop transformed_e0 transformed_e1
      | Direct_app _, _ -> pass_to_accumulator modulo_bop transformed_e0 e1
      | _, Direct_app _ -> pass_to_accumulator modulo_bop e0 transformed_e1
      | _ -> pass_to_accumulator modulo_bop transformed_e0 transformed_e1)
  | _ -> Desugar.Utils.shallow_map_tail_positions rec_transform_tmm e

let rec transform_tmm_decl decl =
  match decl with
  | ValRec (ty, x, e) -> (
      let funargs, body = Desugar.Utils.collect_funargs e in
      match has_tmm_expr x body with
      | None -> [ ValRec (ty, x, Desugar.Utils.replace_funargs funargs body) ]
      | Some modulo_bop ->
          let transformed_body =
            transform_tmm_expr modulo_bop x body |> rewrite_apps modulo_bop x
          in
          let expr_without_acc =
            Desugar.Utils.replace_funargs funargs transformed_body
          in
          let transformed_expr =
            Fun
              ( Typed_ast.TyInt,
                get_expr_type expr_without_acc,
                "acc$",
                expr_without_acc )
          in
          let original_body =
            Direct_app
              ( get_expr_type body,
                [ Typed_ast.TyInt ] @ List.map (fun (_, ty) -> ty) funargs,
                get_expr_type body,
                x ^ "_acc$",
                [ get_identity_element modulo_bop ]
                @ List.map (fun (arg, ty) -> Ident (ty, arg)) funargs )
          in
          let original_expr =
            Desugar.Utils.replace_funargs funargs original_body
          in
          [
            ValRec (ty, x ^ "_acc$", transformed_expr);
            Val (ty, x, original_expr);
          ])
  | And decls -> [ And (List.map transform_tmm_decl decls |> List.flatten) ]
  | _ -> [ decl ]

let transform_tmm_program program =
  let transformed_program =
    List.map transform_tmm_decl program |> List.flatten
  in
  Desugar.Utils.clear_shared_program_seen transformed_program;
  transformed_program
