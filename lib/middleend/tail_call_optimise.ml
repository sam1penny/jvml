(* todo - move this into backend *)

open Desugar.Desugared_ast
open Common

let rec makes_recursive_call_expr fn_name acc e =
  match e with
  | Ident (_, x) -> acc || x = fn_name
  | Direct_app (_, _, _, name, arg_es) ->
      acc || name = fn_name
      || List.fold_left (makes_recursive_call_expr fn_name) acc arg_es
  | _ ->
      Desugar.Utils.fold_left_over_sub_expr
        (fun acc e' -> acc || makes_recursive_call_expr fn_name acc e')
        acc e

let name_generator () =
  let n = ref 0 in
  fun () ->
    let x = !n in
    n := x + 1;
    "temp$" ^ string_of_int x

(* hack to avoid assigning to temporaries when unnecessary.
   Could be fixed with a nice copy propagation phase, instead.
*)

let temporaries_required funargs arg_es =
  List.combine funargs arg_es
  |> List.fold_left
       (fun (args_so_far, temp_required) ((arg_name, _), arg_e) ->
         let fvar_set =
           Desugar.Lambda_lift.free_vars_with_types_expr StringSet.empty arg_e
           |> fun (fvars, _) ->
           StringMap.bindings fvars
           |> List.map (fun (var_name, _) -> var_name)
           |> StringSet.of_list
         in
         let arg_requires_tmp =
           StringSet.inter fvar_set args_so_far |> StringSet.cardinal
           |> fun s -> s > 0
         in
         (StringSet.add arg_name args_so_far, temp_required || arg_requires_tmp))
       (StringSet.empty, false)
  |> fun (_, b) -> b

let generate_argument_assignments name_gen funargs arg_es =
  let temporaries_are_necessary = temporaries_required funargs arg_es in
  if temporaries_are_necessary then
    let temps = List.map (fun (_, ty) -> (name_gen (), ty)) funargs in

    let assign_from_tmps =
      List.combine funargs temps
      |> List.map (fun ((arg_name, arg_ty), (temp_name, temp_ty)) ->
             (arg_name, arg_ty, Ident (temp_ty, temp_name)))
    in
    let assign_to_tmps =
      List.combine temps arg_es
      |> List.map (fun ((arg_name, arg_ty), arg_expr) ->
             (arg_name, arg_ty, arg_expr))
      |> fun l ->
      List.fold_right
        (fun (arg_name, _, arg_expr) e ->
          Let (get_expr_type e, arg_name, arg_expr, e))
        l (Assign_Seq assign_from_tmps)
    in
    assign_to_tmps
  else
    let assign_seq =
      List.combine funargs arg_es
      |> List.map (fun ((arg_name, arg_ty), arg_e) -> (arg_name, arg_ty, arg_e))
    in
    Assign_Seq assign_seq

let rec transform_tail_call_expr_inner name_gen fn_name funargs e =
  let rec_transform_tc =
    transform_tail_call_expr_inner name_gen fn_name funargs
  in
  match e with
  | Int _ | Float _ | Ident _ | Bool _ | Unit | Constr _ | Match_Failure ->
      (e, false)
  | Bop _ | Fun _ | App _ | Tuple _ | TupleGet _ | ConstructorGet _ -> (e, false)
  | Direct_app (_, _, _, name, arg_es) when name = fn_name ->
      let assignments = generate_argument_assignments name_gen funargs arg_es in
      (assignments, true)
  | Direct_app _ -> (e, false)
  | If (ty, e0, e1, e2) ->
      let transformed_e1, did_tail_call_e1 = rec_transform_tc e1 in
      let transformed_e2, did_tail_call_e2 = rec_transform_tc e2 in
      if did_tail_call_e1 || did_tail_call_e2 then
        let e1' =
          if did_tail_call_e1 then transformed_e1 else Break transformed_e1
        in
        let e2' =
          if did_tail_call_e2 then transformed_e2 else Break transformed_e2
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
              else (con, Break trans_case_expr))
            transformed_cases
        in

        let maybe_fallback_expr' =
          Option.map
            (fun (trans_fallback_expr, did_tail_call) ->
              if did_tail_call then trans_fallback_expr
              else Break trans_fallback_expr)
            transformed_fallback
        in
        (Switch (ty, e0, cases', maybe_fallback_expr'), true)
      else (e, false)
  | Shared_Expr (e_ref, _, seen) ->
      if !seen then (e, false)
      else (
        seen := true;
        let transformed_e, did_tail_call = rec_transform_tc !e_ref in
        e_ref := transformed_e;
        (e, did_tail_call))
  | While_true _ | Break _ | Assign_Seq _ ->
      raise
      @@ Failure
           "tail rec constructs should not be present before calling \
            tail_call_optimise"

let transform_tail_call_expr name_gen fn_name funargs e =
  match transform_tail_call_expr_inner name_gen fn_name funargs e with
  | e, true -> While_true e
  | e, false -> e

let rec transform_tail_call_decl decl =
  match decl with
  | ValRec (ty, x, e) ->
      let name_gen = name_generator () in
      let funargs, body = Desugar.Utils.collect_funargs e in
      let transformed_body = transform_tail_call_expr name_gen x funargs body in
      let transformed_expr =
        Desugar.Utils.replace_funargs funargs transformed_body
      in
      let () = Desugar.Utils.clear_shared_expr_seen transformed_expr in
      let transformed_decl =
        match makes_recursive_call_expr x false transformed_expr with
        | true -> ValRec (ty, x, transformed_expr)
        | false -> Val (ty, x, transformed_expr)
      in
      let () = Desugar.Utils.clear_shared_decl_seen transformed_decl in
      transformed_decl
  | And decls -> And (List.map transform_tail_call_decl decls)
  | _ -> decl

let transform_tail_call_program program =
  let transformed_program = List.map transform_tail_call_decl program in
  Desugar.Utils.clear_shared_program_seen transformed_program;
  transformed_program
