(* todo - move this into backend *)

open Desugar.Desugared_ast

let rec transform_tail_call_expr_inner fn_name funargs e =
  let rec_transform_tc = transform_tail_call_expr_inner fn_name funargs in
  match e with
  | Int _ | Ident _ | Bool _ | Unit | Constr _ | Match_Failure -> (e, false)
  | Bop _ | Fun _ | App _ | Tuple _ | TupleGet _ | ConstructorGet _ -> (e, false)
  | Direct_app (_, _, _, name, arg_es) when name = fn_name ->
      let assign_seq =
        List.combine funargs arg_es
        |> List.map (fun ((arg_name, arg_ty), arg_expr) ->
               (arg_name, arg_ty, arg_expr))
      in
      (Assign_Seq assign_seq, true)
  | Direct_app _ -> (e, false)
  | If (ty, e0, e1, e2) ->
      let transformed_e1, did_tail_call_e1 = rec_transform_tc e1 in
      let transformed_e2, did_tail_call_e2 = rec_transform_tc e2 in
      if did_tail_call_e1 || did_tail_call_e2 then
        let e1' =
          if did_tail_call_e1 then transformed_e1 else Return transformed_e1
        in
        let e2' =
          if did_tail_call_e2 then transformed_e2 else Return transformed_e2
        in
        (If (ty, e0, e1', e2'), true)
      else (e, false)
  | Let (ty, x, e0, e1) ->
      let transformed_e1, did_tail_call_e1 = rec_transform_tc e1 in
      (Let (ty, x, e0, transformed_e1), did_tail_call_e1)
  | LetRec (ty, x, e0, e1) ->
      let transformed_e1, did_tail_call_e1 = rec_transform_tc e1 in
      (LetRec (ty, x, e0, transformed_e1), did_tail_call_e1)
  | Seq (ty, es) ->
      let last, rest =
        let reversed = List.rev es in
        (List.hd reversed, List.tl reversed |> List.rev)
      in
      let transformed_last, did_tail_call = rec_transform_tc last in
      (Seq (ty, rest @ [ transformed_last ]), did_tail_call)
  | Switch (ty, e0, cases, maybe_fallback_expr) ->
      let transformed_cases =
        List.map
          (fun (con, case_expr) -> (con, rec_transform_tc case_expr))
          cases
      in
      let transformed_fallback =
        Option.map rec_transform_tc maybe_fallback_expr
      in
      let did_any_tail_calls =
        List.exists
          (fun (_, (_, did_tail_call)) -> did_tail_call)
          transformed_cases
        |> ( || )
             (Option.map
                (fun (_, did_tail_call) -> did_tail_call)
                transformed_fallback
             |> Option.value ~default:false)
      in
      if did_any_tail_calls then
        let cases' =
          List.map
            (fun (con, (trans_case_expr, did_tail_call)) ->
              if did_tail_call then (con, trans_case_expr)
              else (con, Return trans_case_expr))
            transformed_cases
        in

        let maybe_fallback_expr' =
          Option.map
            (fun (trans_fallback_expr, did_tail_call) ->
              if did_tail_call then trans_fallback_expr
              else Return trans_fallback_expr)
            transformed_fallback
        in
        (Switch (ty, e0, cases', maybe_fallback_expr'), true)
      else (e, false)
  | Shared_Expr (e_ref, _) ->
      (* unsafe if expr really is shared - fix with flag on shared_expr *)
      let transformed_e, did_tail_call = rec_transform_tc !e_ref in
      e_ref := transformed_e;
      (e, did_tail_call)
  | While_true _ | Return _ | Assign_Seq _ ->
      raise
      @@ Failure
           "tail rec constructs should not be present before calling \
            tail_call_optimise"

let transform_tail_call_expr fn_name funargs e =
  match transform_tail_call_expr_inner fn_name funargs e with
  | e, true -> While_true e
  | e, false -> e

let transform_tail_call_decl decl =
  match decl with
  | ValRec (ty, x, e) ->
      let funargs, body = Desugar.Utils.collect_funargs e in
      let transformed_body = transform_tail_call_expr x funargs body in
      let transformed_expr =
        Desugar.Utils.replace_funargs funargs transformed_body
      in
      ValRec (ty, x, transformed_expr)
  | _ -> decl

let transform_tail_call_program program =
  List.map transform_tail_call_decl program
