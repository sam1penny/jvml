open Common
open Typing
open Desugared_ast

(* ident -> free var map from var to type *)
type free_var_map = Typed_ast.type_expr StringMap.t StringMap.t

let assemble_fun_ty args_ty ret_ty =
  List.fold_right (fun arg ty -> Typed_ast.TyFun (arg, ty)) args_ty ret_ty

let takeleft _ x _ = Some x

let print_fv_map fv_map =
  StringMap.iter
    (fun v ty -> Printf.printf "%s, %s\n" v (Typed_ast.pp_texpr ty))
    fv_map

let print_fv_maps fv_maps =
  StringMap.iter
    (fun x fv_map ->
      Printf.printf "map for %s\n" x;
      print_fv_map fv_map;
      Printf.printf " --- \n")
    fv_maps

let external_lib = [ "print_$0"; "Nil$"; "Cons$" ] |> StringSet.of_list

let free_vars_with_types_expr bound e =
  let union_list =
    List.fold_left
      (fun (accfree, acc_by_ident) (free, by_ident) ->
        ( StringMap.union takeleft accfree free,
          StringMap.union takeleft acc_by_ident by_ident ))
      (StringMap.empty, StringMap.empty)
  in
  let rec aux bound free e =
    let rec_aux = aux bound free in
    match e with
    | Int _ | Float _ | String _ | Bool _ | Unit | Constr _ ->
        (free, StringMap.empty)
    | Ident (ty, i) ->
        ( (if StringSet.mem i bound then free else StringMap.add i ty free),
          StringMap.empty )
    | Bop (_, e0, _, e1) ->
        let free0, fvs_by_ident0 = rec_aux e0 in
        let free1, fvs_by_ident1 = rec_aux e1 in
        let free = StringMap.union takeleft free0 free1 in
        let fvs_by_ident =
          StringMap.union takeleft fvs_by_ident0 fvs_by_ident1
        in
        (free, fvs_by_ident)
    | If (_, e0, e1, e2) ->
        let free0, fvs_by_ident0 = rec_aux e0 in
        let free1, fvs_by_ident1 = rec_aux e1 in
        let free2, fvs_by_ident2 = rec_aux e2 in
        let free =
          StringMap.union takeleft free0 free1 |> StringMap.union takeleft free2
        in
        let fvs_by_ident =
          StringMap.union takeleft fvs_by_ident0 fvs_by_ident1
          |> StringMap.union takeleft fvs_by_ident2
        in
        (free, fvs_by_ident)
    | Fun (_, _, x, e) ->
        let free, fvs_by_ident = rec_aux e in
        (StringMap.remove x free, fvs_by_ident)
    | App (_, e0, e1) ->
        let free0, fvs_by_ident0 = rec_aux e0 in
        let free1, fvs_by_ident1 = rec_aux e1 in
        let free = StringMap.union takeleft free0 free1 in
        let fvs_by_ident =
          StringMap.union takeleft fvs_by_ident0 fvs_by_ident1
        in
        (free, fvs_by_ident)
    | Direct_app (_, args_ty, fun_arg_ty, name, es) ->
        List.map rec_aux es |> union_list |> fun (accfree, acc_by_indent) ->
        ( StringMap.union takeleft accfree
            (StringMap.singleton name (assemble_fun_ty args_ty fun_arg_ty)),
          acc_by_indent )
    | Tuple (_, es) -> List.map rec_aux es |> union_list
    | Let (_, x, (Fun _ as e0), e1) ->
        let free0, fvs_by_ident0 = aux bound free e0 in
        let free1, fvs_by_ident1 = aux bound free e1 in
        let free1 = StringMap.remove x free1 in
        (* remove bound value from free1 (still might be free in free0)*)
        let fvs_by_ident =
          StringMap.union takeleft fvs_by_ident0 fvs_by_ident1
          |> StringMap.add x free0
        in
        (StringMap.union takeleft free0 free1, fvs_by_ident)
    | Let (_, x, e0, e1) ->
        let free0, fvs_by_ident0 = aux bound free e0 in
        let free1, fvs_by_ident1 = aux bound free e1 in
        let free1 = StringMap.remove x free1 in
        (* remove bound value from free1 (still might be free in free0)*)
        let fvs_by_ident =
          StringMap.union takeleft fvs_by_ident0 fvs_by_ident1
        in
        (StringMap.union takeleft free0 free1, fvs_by_ident)
    | LetRec (_, x, e0, e1) ->
        let free0, fvs_by_ident0 = aux bound free e0 in
        let free1, fvs_by_ident1 = aux bound free e1 in
        let free0 = StringMap.remove x free0 in
        let free1 = StringMap.remove x free1 in
        let fvs_by_ident =
          StringMap.union takeleft fvs_by_ident0 fvs_by_ident1
          |> StringMap.add x free0
        in
        (StringMap.union takeleft free0 free1, fvs_by_ident)
    | Seq (_, es) -> List.map rec_aux es |> union_list
    (* index is undecidable, so look at entire e *)
    | TupleGet (_, _, e) -> aux bound free e
    | ConstructorGet (_, _, e) -> aux bound free e
    | Switch (_, e, cases, fallback_opt) -> (
        let free_e, by_ident_e = rec_aux e in
        List.map (fun (_, case_expr) -> case_expr) cases
        |> List.map rec_aux |> union_list
        |> fun (free, by_ident) ->
        ( StringMap.union takeleft free_e free,
          StringMap.union takeleft by_ident_e by_ident )
        |> fun (accfree, acc_by_indent) ->
        match fallback_opt with
        | None -> (accfree, acc_by_indent)
        | Some fallback_expr ->
            let fallback_free, fallback_by_ident = rec_aux fallback_expr in
            ( StringMap.union takeleft accfree fallback_free,
              StringMap.union takeleft acc_by_indent fallback_by_ident ))
    | Match_Failure -> (StringMap.empty, StringMap.empty)
    | Shared_Expr (e, _, _) -> rec_aux !e
    | While_true _ | Break _ | Assign_Seq _ ->
        raise
        @@ Failure "tail rec constructs should not be present in lambda_lift"
  in

  aux bound StringMap.empty e

let free_vars_with_types_decl bound decl =
  match decl with
  | Val (_, x, e) ->
      free_vars_with_types_expr bound e |> fun (fv_val, fv_maps) ->
      StringMap.add x fv_val fv_maps |> fun fv_maps ->
      (fv_maps, StringSet.add x bound)
  | ValRec (_, x, e) ->
      let bound' = StringSet.add x bound in
      free_vars_with_types_expr bound' e |> fun (fv_val, fv_map) ->
      StringMap.add x fv_val fv_map |> fun fv_map -> (fv_map, bound')
  | Type _ -> (StringMap.empty, bound)
  | And _ ->
      raise @@ Failure "mutual recursion should not occur before lambda lifting"

let free_vars_with_types_program program =
  List.fold_left
    (fun (fv_map, top_level) decl ->
      let d_fv_map, new_top_level = free_vars_with_types_decl top_level decl in
      (StringMap.union takeleft fv_map d_fv_map, new_top_level))
    (StringMap.empty, external_lib)
    program
  |> fun (fv_map, _) -> fv_map

let lift_to_valrec name funargs body =
  let fun_body = Utils.replace_funargs funargs body in
  ValRec (get_expr_type fun_body, name, fun_body)

let lift_to_val name funargs body =
  let fun_body = Utils.replace_funargs funargs body in
  Val (get_expr_type fun_body, name, fun_body)

let apply_captured_vars lifted_fun free_vars =
  List.fold_left
    (fun acc (arg, arg_ty) -> App (get_expr_type acc, acc, Ident (arg_ty, arg)))
    lifted_fun free_vars

(*
lambda lifting makes a identifier static/toplevel,
so it can now be removed from free variable mappings.
*)
let remove_from_fv_tbls key fv_tbls =
  Hashtbl.iter (fun _ fv_tbl -> Hashtbl.remove fv_tbl key) fv_tbls

(* dodgy bodge to set flag if mutual recursion is actually required *)
let mutual_recursion_required = ref false

let rec lift_lambdas_expr decl_name free_var_tbl e =
  let rec_lift_lambdas_expr = lift_lambdas_expr decl_name free_var_tbl in
  let rec_lift_list es =
    let lifted_es = List.map rec_lift_lambdas_expr es in
    let lifted_defs =
      List.map (fun (def, _) -> def) lifted_es |> List.flatten
    in
    let lifted_es = List.map (fun (_, e) -> e) lifted_es in
    (lifted_defs, lifted_es)
  in
  match e with
  | Int _ | Float _ | String _ | Bool _ | Unit | Constr _ | Match_Failure ->
      ([], e)
  | Ident (_, i) as e_ident -> (
      match Hashtbl.find_opt free_var_tbl i with
      | None -> ([], e_ident)
      | Some fv_tbl ->
          let freevars = Hashtbl.to_seq fv_tbl |> List.of_seq in
          ([], apply_captured_vars e_ident freevars))
  | Bop (ty, e0, bop, e1) ->
      let defs0, e0 = rec_lift_lambdas_expr e0 in
      let defs1, e1 = rec_lift_lambdas_expr e1 in
      (defs0 @ defs1, Bop (ty, e0, bop, e1))
  | If (ty, e0, e1, e2) ->
      let defs0, e0 = rec_lift_lambdas_expr e0 in
      let defs1, e1 = rec_lift_lambdas_expr e1 in
      let defs2, e2 = rec_lift_lambdas_expr e2 in
      (defs0 @ defs1 @ defs2, If (ty, e0, e1, e2))
  (* todo - anonymous lambda lifting *)
  | Fun (t0, t1, x, e) ->
      let defs, e = rec_lift_lambdas_expr e in
      (defs, Fun (t0, t1, x, e))
  | App (ty, e0, e1) ->
      let defs0, e0 = rec_lift_lambdas_expr e0 in
      let defs1, e1 = rec_lift_lambdas_expr e1 in
      (defs0 @ defs1, App (ty, e0, e1))
  | Direct_app _ ->
      raise
      @@ Failure
           "illegal state - should have ran lambda_lift before direct_calls"
  | Let (_, x, (Fun _ as e0), e1) ->
      let funargs, body = Utils.collect_funargs e0 in
      let freevars =
        Hashtbl.find free_var_tbl x |> Hashtbl.to_seq |> List.of_seq
      in
      let lifted_args = freevars @ funargs in
      let defs0, body0 = rec_lift_lambdas_expr body in
      let lifted_val = lift_to_val x lifted_args body0 in
      remove_from_fv_tbls x free_var_tbl;
      let defs1, e1 = rec_lift_lambdas_expr e1 in
      ([ lifted_val ] @ defs0 @ defs1, e1)
  | Let (ty, x, e0, e1) ->
      let defs0, e0 = rec_lift_lambdas_expr e0 in
      let defs1, e1 = rec_lift_lambdas_expr e1 in
      (defs0 @ defs1, Let (ty, x, e0, e1))
  | LetRec (_, x, e0, e1) ->
      let funargs, body = Utils.collect_funargs e0 in
      let freevars =
        Hashtbl.find free_var_tbl x |> Hashtbl.to_seq |> List.of_seq
      in
      let lifted_args = freevars @ funargs in
      let defs0, body0 = rec_lift_lambdas_expr body in
      let lifted_valrec = lift_to_valrec x lifted_args body0 in
      remove_from_fv_tbls x free_var_tbl;
      let defs1, e1 = rec_lift_lambdas_expr e1 in

      let freevars_in_e0, _ = free_vars_with_types_expr StringSet.empty e0 in

      if StringMap.mem decl_name freevars_in_e0 then
        mutual_recursion_required := true
      else ();
      ([ lifted_valrec ] @ defs0 @ defs1, e1)
  | Tuple (ty, es) ->
      let lifted_defs, lifted_es = rec_lift_list es in
      (lifted_defs, Tuple (ty, lifted_es))
  | Seq (ty, es) ->
      let lifted_defs, lifted_es = rec_lift_list es in
      (lifted_defs, Seq (ty, lifted_es))
  | TupleGet (ty, i, e) ->
      let defs, e = rec_lift_lambdas_expr e in
      (defs, TupleGet (ty, i, e))
  | ConstructorGet (ty, cname, e) ->
      let defs, e = rec_lift_lambdas_expr e in
      (defs, ConstructorGet (ty, cname, e))
  | Switch (ty, e, cases, maybe_fallback_expr) ->
      let defs, e = rec_lift_lambdas_expr e in
      let cons = List.map (fun (con, _) -> con) cases in
      let case_defs, lifted_case_exprs =
        List.map (fun (_, case_expr) -> case_expr) cases |> rec_lift_list
      in
      let lifted_cases = List.combine cons lifted_case_exprs in
      let fallback_defs, maybe_lifted_fallback_expr =
        Option.map
          (fun fallback_expr ->
            let fallback_defs, lifted_fallback_expr =
              rec_lift_lambdas_expr fallback_expr
            in
            (fallback_defs, Some lifted_fallback_expr))
          maybe_fallback_expr
        |> Option.value ~default:([], None)
      in
      ( defs @ case_defs @ fallback_defs,
        Switch (ty, e, lifted_cases, maybe_lifted_fallback_expr) )
  | Shared_Expr (e_ref, label_ref, seen) as shared_expr ->
      if !seen then ([], shared_expr)
      else (
        seen := true;
        let defs, e = rec_lift_lambdas_expr !e_ref in
        e_ref := e;
        (defs, Shared_Expr (e_ref, label_ref, seen)))
  | While_true _ | Break _ | Assign_Seq _ ->
      raise
      @@ Failure "tail rec constructs should not be present in lambda_lift"

let lift_lambdas_decl free_var_map decl =
  match decl with
  | Val (ty, x, e) ->
      let lifted_decls, e = lift_lambdas_expr x free_var_map e in
      lifted_decls @ [ Val (ty, x, e) ]
  | ValRec (ty, x, e) ->
      mutual_recursion_required := false;
      let lifted_decls, e = lift_lambdas_expr x free_var_map e in
      if !mutual_recursion_required then
        [ And (lifted_decls @ [ ValRec (ty, x, e) ]) ]
      else lifted_decls @ [ ValRec (ty, x, e) ]
  | Type _ -> [ decl ]
  | And _ ->
      raise @@ Failure "mutual recursion should not occur before lambda lifting"

let to_hashtbl fv_map =
  StringMap.to_seq fv_map
  |> Seq.map (fun (x, m) -> (x, StringMap.to_seq m |> Hashtbl.of_seq))
  |> Hashtbl.of_seq

let lift_lambdas_program program =
  let fv_map = free_vars_with_types_program program in
  let fv_tbl = to_hashtbl fv_map in
  let lifted_program =
    List.map (lift_lambdas_decl fv_tbl) program |> List.flatten
  in
  Utils.clear_shared_program_seen lifted_program;
  lifted_program
