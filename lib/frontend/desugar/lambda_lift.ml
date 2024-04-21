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
  let open Desugared_ast in
  let takeleft _ x _ = Some x in
  let rec aux bound free = function
    | Int _ | Float _ | String _ | Bool _ | Unit | Constr _ -> free
    | Ident (ty, i) ->
        if StringSet.mem i bound then free else StringMap.add i ty free
    | Bop (_, e0, _, e1) ->
        StringMap.union takeleft (aux bound free e0) (aux bound free e1)
    | Uop (_, _, e) -> aux bound free e
    | If (_, e0, e1, e2) ->
        StringMap.union takeleft (aux bound free e0) (aux bound free e1)
        |> StringMap.union takeleft (aux bound free e2)
    | Fun (_, _, x, e) ->
        let bound' = StringSet.add x bound in
        aux bound' free e
    | App (_, e0, e1) ->
        StringMap.union takeleft (aux bound free e0) (aux bound free e1)
    | Direct_app (_, args_ty, fun_ret_ty, name, es) ->
        let acc =
          if StringSet.mem name bound then StringMap.empty
          else StringMap.singleton name (assemble_fun_ty args_ty fun_ret_ty)
        in
        List.map (aux bound free) es
        |> List.fold_left (StringMap.union takeleft) acc
    | Tuple (_, es) ->
        List.map (aux bound free) es
        |> List.fold_left (StringMap.union takeleft) StringMap.empty
    | Let (_, x, e0, e1) ->
        let free_e0 = aux bound free e0 in
        let bound' = StringSet.add x bound in
        StringMap.union takeleft free_e0 (aux bound' free e1)
    | LetRec (_, x, e0, e1) ->
        let bound' = StringSet.add x bound in
        StringMap.union takeleft (aux bound' free e0) (aux bound' free e1)
    | Seq (_, es) ->
        List.map (aux bound free) es
        |> List.fold_left (StringMap.union takeleft) StringMap.empty
    (* index is undecidable, so look at entire e *)
    | TupleGet (_, _, e) -> aux bound free e
    | ConstructorGet (_, _, e) -> aux bound free e
    | Switch (_, e, cases, fallback_opt) ->
        let free_e = aux bound free e in
        List.map (fun (_, case_expr) -> case_expr) cases
        |> List.map (aux bound free)
        |> List.fold_left (StringMap.union takeleft) free_e
        |> StringMap.union takeleft
             (Option.map (fun e -> aux bound free e) fallback_opt
             |> Option.value ~default:StringMap.empty)
    | Match_Failure -> StringMap.empty
    | Shared_Expr (e, _, _) -> aux bound free !e
    | While_true e -> aux bound free e
    | Break e -> aux bound free e
    | Assign_Seq assignments ->
        List.map
          (fun (x, ty, e) ->
            aux bound free e |> fun m ->
            if StringSet.mem x bound then StringMap.add x ty m else m)
          assignments
        |> List.fold_left (StringMap.union takeleft) StringMap.empty
    | Hole | Set_Tuple _ -> raise @@ Failure "todo1"
  in

  aux bound StringMap.empty e

let apply_captured_vars lifted_fun free_vars =
  List.fold_left
    (fun acc (arg, arg_ty) -> App (get_expr_type acc, acc, Ident (arg_ty, arg)))
    lifted_fun free_vars

let rec lift_lambdas_expr decl_name toplevel lifted_freevars_tbl e =
  let rec_lift_lambda_without_bound = lift_lambdas_expr decl_name in
  let rec_lift_lambdas_expr =
    lift_lambdas_expr decl_name toplevel lifted_freevars_tbl
  in
  let rec_lift_list es =
    let lifted_es = List.map rec_lift_lambdas_expr es in
    let lifted_defs =
      List.map (fun (def, _) -> def) lifted_es |> List.flatten
    in
    let lifted_es = List.map (fun (_, e) -> e) lifted_es in
    (lifted_defs, lifted_es)
  in
  match e with
  | Int _ | Float _ | String _ | Bool _ | Unit | Constr _ | Match_Failure | Hole
    ->
      ([], e)
  | Ident (_, i) as e_ident -> (
      match Hashtbl.find_opt lifted_freevars_tbl i with
      | None -> ([], e_ident)
      | Some freevars -> ([], apply_captured_vars e_ident freevars))
  | Bop (ty, e0, bop, e1) ->
      let defs0, e0 = rec_lift_lambdas_expr e0 in
      let defs1, e1 = rec_lift_lambdas_expr e1 in
      (defs0 @ defs1, Bop (ty, e0, bop, e1))
  | Uop (ty, uop, e) ->
      let defs, e' = rec_lift_lambdas_expr e in
      (defs, Uop (ty, uop, e'))
  | If (ty, e0, e1, e2) ->
      let defs0, e0 = rec_lift_lambdas_expr e0 in
      let defs1, e1 = rec_lift_lambdas_expr e1 in
      let defs2, e2 = rec_lift_lambdas_expr e2 in
      (defs0 @ defs1 @ defs2, If (ty, e0, e1, e2))
  (* todo - anonymous lambda lifting *)
  | Fun (t0, t1, x, e) ->
      let defs, e =
        rec_lift_lambda_without_bound toplevel lifted_freevars_tbl e
      in
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
        free_vars_with_types_expr toplevel e0 |> StringMap.to_seq |> List.of_seq
      in
      let lifted_args = freevars @ funargs in

      let defs0, body0 =
        rec_lift_lambda_without_bound (StringSet.add x toplevel)
          lifted_freevars_tbl body
      in

      let lifted_body = Utils.replace_funargs lifted_args body0 in
      let lifted_val = Val (get_expr_type lifted_body, x, lifted_body) in

      Hashtbl.add lifted_freevars_tbl x freevars;

      let defs1, e1 =
        rec_lift_lambda_without_bound (StringSet.add x toplevel)
          lifted_freevars_tbl e1
      in
      ([ lifted_val ] @ defs0 @ defs1, e1)
  | Let (ty, x, e0, e1) ->
      let defs0, e0 = rec_lift_lambdas_expr e0 in
      let defs1, e1 = rec_lift_lambdas_expr e1 in
      (defs0 @ defs1, Let (ty, x, e0, e1))
  | LetRec (_, x, e0, e1) ->
      let funargs, body = Utils.collect_funargs e0 in
      let freevars =
        free_vars_with_types_expr (StringSet.add x toplevel) e0
        |> StringMap.to_seq |> List.of_seq
      in

      Hashtbl.add lifted_freevars_tbl x freevars;

      let defs0, body0 =
        rec_lift_lambda_without_bound (StringSet.add x toplevel)
          lifted_freevars_tbl body
      in

      let lifted_args = freevars @ funargs in

      let lifted_body = Utils.replace_funargs lifted_args body0 in
      let lifted_valrec = ValRec (get_expr_type lifted_body, x, lifted_body) in
      let defs1, e1 =
        rec_lift_lambda_without_bound (StringSet.add x toplevel)
          lifted_freevars_tbl e1
      in

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
  | Set_Tuple (e0, e1, e2) ->
      let defs0, e0' = rec_lift_lambdas_expr e0 in
      let defs1, e1' = rec_lift_lambdas_expr e1 in
      let defs2, e2' = rec_lift_lambdas_expr e2 in
      (defs0 @ defs1 @ defs2, Set_Tuple (e0', e1', e2'))

let expr_idents e =
  let rec inner acc e =
    match e with
    | Ident (_, x) -> StringSet.add x acc
    | _ -> Utils.fold_left_over_sub_expr inner acc e
  in
  let res = inner StringSet.empty e in
  Utils.clear_shared_expr_seen e;
  res

(*
check if mutual recursion is required to avoid creating unnecessary AND

could be more precise, rather than all or nothing, but simple enough for most cases
*)
let mutual_recursion_required lifted_decls =
  let intersection s1 s2 = StringSet.inter s1 s2 |> StringSet.cardinal > 0 in
  List.fold_left
    (fun (lifted_decls, exists) decl ->
      if exists then (lifted_decls, true)
      else
        match decl with
        | Val (_, x, e) | ValRec (_, x, e) ->
            ( StringSet.add x lifted_decls,
              intersection lifted_decls (expr_idents e) )
        | _ -> (lifted_decls, false))
    (StringSet.empty, false) (List.rev lifted_decls)
  |> fun (_, required) -> required

let lift_lambdas_decl toplevel lifted_freevars_tbl decl =
  match decl with
  | Val (ty, x, e) ->
      let lifted_decls, e =
        lift_lambdas_expr x toplevel lifted_freevars_tbl e
      in
      let toplevel' = StringSet.add x toplevel in
      (lifted_decls @ [ Val (ty, x, e) ], toplevel')
  | ValRec (ty, x, e) ->
      let toplevel' = StringSet.add x toplevel in
      let lifted_decls, e =
        lift_lambdas_expr x toplevel' lifted_freevars_tbl e
      in
      Utils.clear_shared_expr_seen e;
      List.iter Utils.clear_shared_decl_seen lifted_decls;
      let lifted_decls = lifted_decls @ [ ValRec (ty, x, e) ] in
      if mutual_recursion_required lifted_decls then
        ([ And lifted_decls ], toplevel')
      else (lifted_decls, toplevel')
  | Type _ -> ([ decl ], toplevel)
  | And _ ->
      raise @@ Failure "mutual recursion should not occur before lambda lifting"

let to_hashtbl fv_map =
  StringMap.to_seq fv_map
  |> Seq.map (fun (x, m) -> (x, StringMap.to_seq m |> Hashtbl.of_seq))
  |> Hashtbl.of_seq

let lift_lambdas_program program =
  let lifted_freevars_tbl = Hashtbl.create 10 in
  let lifted_program =
    List.fold_left
      (fun (new_program, toplevel) decl ->
        let new_decls, toplevel' =
          lift_lambdas_decl toplevel lifted_freevars_tbl decl
        in
        (new_program @ new_decls, toplevel'))
      ([], StringSet.empty) program
    |> fun (p, _) -> p
  in
  Utils.clear_shared_program_seen lifted_program;
  lifted_program
