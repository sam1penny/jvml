open Desugar.Desugared_ast
open Common

(*
TODO - bug occurs when we beta reduce, then code doesn't exist.
*)
let rec beta_reduce_expr e =
  match e with
  | App (_, Fun (_, t1, x, e0), e1) ->
      Let (t1, x, beta_reduce_expr e1, beta_reduce_expr e0)
  | _ -> Desugar.Utils.map_over_sub_expr beta_reduce_expr e

let beta_reduce_decl d =
  let reduced_program = Desugar.Utils.map_over_decl_exprs beta_reduce_expr d in
  let () = Desugar.Utils.clear_shared_decl_seen reduced_program in
  reduced_program

(* can refactor into a fold *)
let rec size_expr_helper e =
  match e with
  | Int _ | Float _ | String _ | Bool _ | Unit | Ident _ | Constr _
  | Match_Failure | Hole ->
      1
  | Bop (_, e0, _, e1) -> 1 + size_expr_helper e0 + size_expr_helper e1
  | Uop (_, _, e) -> 1 + size_expr_helper e
  | If (_, e0, e1, e2) | Set_Tuple (e0, e1, e2) ->
      1 + size_expr_helper e0 + size_expr_helper e1 + size_expr_helper e2
  | Fun (_, _, _, e) -> 1 + size_expr_helper e
  | App (_, e0, e1) -> 1 + size_expr_helper e0 + size_expr_helper e1
  | Direct_app (_, _, _, _, es) ->
      1 + (List.map size_expr_helper es |> List.fold_left ( + ) 0)
  | Tuple (_, es) | Seq (_, es) ->
      1 + (List.map size_expr_helper es |> List.fold_left ( + ) 0)
  | Let (_, _, e0, e1) | LetRec (_, _, e0, e1) ->
      1 + size_expr_helper e0 + size_expr_helper e1
  | TupleGet (_, _, e) -> 1 + size_expr_helper e
  | ConstructorGet (_, _, e) -> 1 + size_expr_helper e
  | Switch (_, Ident _, cases, maybe_fallback_expr) ->
      (List.map (fun (_, case_expr) -> size_expr_helper case_expr) cases
      |> List.fold_left ( + ) 0)
      + (Option.map size_expr_helper maybe_fallback_expr
        |> Option.value ~default:0)
  | Switch (_, e, cases, maybe_fallback_expr) ->
      1 + size_expr_helper e
      + (List.map (fun (_, case_expr) -> size_expr_helper case_expr) cases
        |> List.fold_left ( + ) 0)
      + (Option.map size_expr_helper maybe_fallback_expr
        |> Option.value ~default:0)
  | Shared_Expr (e_ref, _, seen) ->
      if !seen then 0
      else (
        seen := true;
        size_expr_helper !e_ref)
  | While_true e | Break e -> 1 + size_expr_helper e
  | Assign_Seq assigns ->
      1
      + (List.map (fun (_, _, e) -> size_expr_helper e) assigns
        |> List.fold_left ( + ) 0)

let size_expr e =
  let size = size_expr_helper e in
  let () = Desugar.Utils.clear_shared_expr_seen e in
  size

let rec copy_shared_exprs_helper already_copied e =
  match e with
  | Shared_Expr (e_ref, _, _) -> (
      match Hashtbl.find_opt already_copied e_ref with
      | None ->
          let e_ref' = ref !e_ref in
          let shared_expr = Shared_Expr (e_ref', ref None, ref false) in
          Hashtbl.add already_copied e_ref shared_expr;
          shared_expr
      | Some shared_expr -> shared_expr)
  | _ ->
      Desugar.Utils.map_over_sub_expr
        (copy_shared_exprs_helper already_copied)
        e

let copy_shared_exprs e = copy_shared_exprs_helper (Hashtbl.create 10) e

let rec build_type_mapping tbl general_ty special_ty =
  let open Typing.Typed_ast in
  match (general_ty, special_ty) with
  | TyInt, TyInt
  | TyBool, TyBool
  | TyUnit, TyUnit
  | TyString, TyString
  | TyFloat, TyFloat ->
      ()
  | TyVar v, special -> Hashtbl.add tbl v special
  | TyFun (general_t0, general_t1), TyFun (special_t0, special_t1) ->
      build_type_mapping tbl general_t0 special_t0;
      build_type_mapping tbl general_t1 special_t1
  | TyTuple general_tys, TyTuple special_tys ->
      List.combine general_tys special_tys
      |> List.iter (fun (g, s) -> build_type_mapping tbl g s)
  | TyCustom (general_tys, _), TyCustom (special_tys, _) ->
      List.combine general_tys special_tys
      |> List.iter (fun (g, s) -> build_type_mapping tbl g s)
  | _ ->
      raise
      @@ Failure
           (Printf.sprintf "illegal state, types %s and %s don't match."
              (pp_texpr general_ty) (pp_texpr special_ty))

let get_or_fail tbl x =
  Hashtbl.find_opt tbl x |> function
  | Some v -> v
  | None -> raise @@ Failure (Printf.sprintf "failed to find %s in tbl" x)

let instantiate_type called_type code =
  let type_mapping = Hashtbl.create 10 in
  let _ = build_type_mapping type_mapping (get_expr_type code) called_type in

  Desugar.Utils.map_over_expr_texprs
    (Typing.Infer.map_over_texpr_vars (fun x ->
         Hashtbl.find_opt type_mapping x
         |> Option.value ~default:(Typing.Typed_ast.TyVar x)))
    code

let poor_mans_effect_safety e =
  let rec poor_mans_effect_safety_inner acc e =
    match e with
    | App _ | Direct_app _ -> false (* assume the worst *)
    | _ ->
        Desugar.Utils.fold_left_over_sub_expr
          (fun acc e' -> acc && poor_mans_effect_safety_inner acc e')
          acc e
  in
  let is_safe = poor_mans_effect_safety_inner true e in
  Desugar.Utils.clear_shared_expr_seen e;
  is_safe

let increment_tbl_if_exist tbl key =
  match Hashtbl.find_opt tbl key with
  | None -> ()
  | Some v -> Hashtbl.add tbl key (v + 1)

(*
todo - add bindings to hashtable and only increment if identifier is actually in the table
*)
let rec safety_of_bindings_expr safe_cnts unsafe_cnts safe_vars e =
  let rec_safety_of_bindings_expr =
    safety_of_bindings_expr safe_cnts unsafe_cnts safe_vars
  in
  match e with
  | Ident (_, x) ->
      if Hashset.mem safe_vars x then increment_tbl_if_exist safe_cnts x
      else increment_tbl_if_exist unsafe_cnts x
  | Direct_app (_, _, _, x, arg_es) ->
      if Hashset.mem safe_vars x then increment_tbl_if_exist safe_cnts x
      else increment_tbl_if_exist unsafe_cnts x;
      List.iter rec_safety_of_bindings_expr arg_es
  | Let (_, x, e0, e1) when poor_mans_effect_safety e0 ->
      let () = rec_safety_of_bindings_expr e0 in
      let () = Hashset.add safe_vars x in
      let () = Hashtbl.add safe_cnts x 0 in
      let () = Hashtbl.add unsafe_cnts x 0 in
      safety_of_bindings_expr safe_cnts unsafe_cnts safe_vars e1
  (* clear set of variables that are safe as we cross a lambda boundary *)
  | Fun (_, _, _, e) ->
      safety_of_bindings_expr safe_cnts unsafe_cnts (Hashset.create 10) e
  | _ -> Desugar.Utils.iter_over_sub_expr rec_safety_of_bindings_expr e

let rec safety_of_bindings_decl safe_cnts unsafe_cnts safe_vars d =
  match d with
  | Val (_, x, e) ->
      let _ = safety_of_bindings_expr safe_cnts unsafe_cnts safe_vars e in
      let () = Hashset.add safe_vars x in
      let () = Hashtbl.add safe_cnts x 0 in
      (* for now, say that toplevel bindings are unsafe, since they are exported *)
      Hashtbl.add unsafe_cnts x 1
  | ValRec (_, _, e) ->
      (* ignore inlining recursive functions *)
      safety_of_bindings_expr safe_cnts unsafe_cnts safe_vars e
  | Type _ -> ()
  | And decls ->
      List.iter (safety_of_bindings_decl safe_cnts unsafe_cnts safe_vars) decls

let safety_of_bindings_program program =
  let safe_cnts = Hashtbl.create 10 in
  let unsafe_cnts = Hashtbl.create 10 in
  let safe_vars = Hashtbl.create 10 in
  let () =
    List.iter (safety_of_bindings_decl safe_cnts unsafe_cnts safe_vars) program
  in
  let () = Desugar.Utils.clear_shared_program_seen program in
  (safe_cnts, unsafe_cnts)

type binding_occurrence = Dead | OnceSafe | OnceUnsafe | MultiUnsafe
[@@deriving show]

let occurrence_analysis program =
  let occurrence_tbl = Hashtbl.create 10 in
  let safe_cnts, unsafe_cnts = safety_of_bindings_program program in
  let keys =
    StringSet.union
      (Hashtbl.to_seq_keys safe_cnts |> StringSet.of_seq)
      (Hashtbl.to_seq_keys unsafe_cnts |> StringSet.of_seq)
  in
  StringSet.iter
    (fun var ->
      match (Hashtbl.find safe_cnts var, Hashtbl.find unsafe_cnts var) with
      | 0, 0 -> Hashtbl.add occurrence_tbl var Dead
      | 1, 0 -> Hashtbl.add occurrence_tbl var OnceSafe
      | 0, 1 -> Hashtbl.add occurrence_tbl var OnceUnsafe
      | _ -> Hashtbl.add occurrence_tbl var MultiUnsafe)
    keys;
  occurrence_tbl

type context = OtherCtx | AppCtx of context | CaseCtx of context

(*
identifies the kind of expressions that require only a small piece of work,
so that inlining will duplicate minimal work
(or potentially less -- either is a simple lookup)

equivalent to a copy-propagation pass
*)
let is_value e =
  match e with
  | Fun _ | Constr _ | Int _ | Bool _ | Ident _ | Float _ | String _ -> true
  | _ -> false

let very_boring ctx = match ctx with OtherCtx -> true | _ -> false

let rec num_applied ctx =
  match ctx with AppCtx c -> 1 + num_applied c | _ -> 0

let compute_inlining_score code_tbl x ctx =
  size_expr
    (Hashtbl.find_opt code_tbl x |> function
     | Some c -> c
     | None -> raise @@ Failure (Printf.sprintf "failed to find %s" x))
  - num_applied ctx

let small_enough _ code_tbl x ctx =
  if
    compute_inlining_score code_tbl x ctx
    < !Common.Config.inlining_score_threshold
  then true
  else false

let consider_inline size_tbl code_tbl occurrence_tbl context x =
  match Hashtbl.find_opt occurrence_tbl x with
  | None -> false
  (* identifier is either function argument
     or a side-effecting term, not suitable for inlining *)
  (* todo - if we unconditionally inline as in the paper, we can automatically apply deadcode elimination.contents

     Or, we can just add a check to a 'let' expression
  *)
  | Some Dead -> raise @@ Failure "inlining dead expression"
  | Some OnceSafe -> true
  | Some OnceUnsafe ->
      is_value
        (Hashtbl.find_opt code_tbl x |> function
         | Some c -> c
         | None ->
             raise @@ Failure (Printf.sprintf "fail to find %s in code_tbl" x))
      && not (very_boring context)
  | Some MultiUnsafe ->
      is_value
        (Hashtbl.find_opt code_tbl x |> function
         | Some c -> c
         | None ->
             raise @@ Failure (Printf.sprintf "fail to find %s in code_tbl" x))
      && small_enough size_tbl code_tbl x context

let rename_bindings most_recent_version e =
  Desugar.Utils.clear_shared_expr_seen e;
  let e' =
    Desugar.Unique_names.rename_expr most_recent_version (Hashtbl.create 10) e
  in
  Desugar.Utils.clear_shared_expr_seen e;
  e'

let rec inline_expr size_tbl code_tbl occurrence_tbl most_recent_version context
    e =
  let rec_inline_expr_without_ctx =
    inline_expr size_tbl code_tbl occurrence_tbl most_recent_version
  in
  match e with
  | Int _ | Float _ | String _ | Bool _ | Unit | Constr _ | Match_Failure | Hole
    ->
      e
  | Ident (ty, x) ->
      if consider_inline size_tbl code_tbl occurrence_tbl context x then
        Hashtbl.find_opt code_tbl x
        |> (function
             | Some c -> c
             | None ->
                 raise
                 @@ Failure (Printf.sprintf "fail to find %s in code_tbl" x))
        |> copy_shared_exprs |> instantiate_type ty
        |> rename_bindings most_recent_version
      else e
  | Direct_app (ty, arg_tys, ret_ty, name, arg_es) ->
      let app_contexts =
        List.fold_left (fun ctx _ -> AppCtx ctx) context arg_es
      in
      let specialised_ty =
        List.fold_right
          (fun arg_e acc -> Typing.Typed_ast.TyFun (get_expr_type arg_e, acc))
          arg_es ty
      in
      if consider_inline size_tbl code_tbl occurrence_tbl app_contexts name then
        let inlined_method =
          get_or_fail code_tbl name |> copy_shared_exprs
          |> instantiate_type specialised_ty
          |> rename_bindings most_recent_version
        in
        let applications =
          List.fold_left
            (fun acc arg ->
              let app_ty =
                match get_expr_type acc with
                | TyFun (_, t1) -> t1
                | _ -> raise @@ Failure "direct_app supplies too many arguments"
              in
              App (app_ty, acc, arg))
            inlined_method arg_es
        in
        applications
      else Direct_app (ty, arg_tys, ret_ty, name, arg_es)
  | Bop (ty, e0, bop, e1) ->
      let e0' = rec_inline_expr_without_ctx OtherCtx e0 in
      let e1' = rec_inline_expr_without_ctx OtherCtx e1 in
      Bop (ty, e0', bop, e1')
  | Uop (ty, uop, e) ->
      let e' = rec_inline_expr_without_ctx OtherCtx e in
      Uop (ty, uop, e')
  | If (ty, e0, e1, e2) ->
      let e0' = rec_inline_expr_without_ctx (CaseCtx context) e0 in
      let e1' = rec_inline_expr_without_ctx context e1 in
      let e2' = rec_inline_expr_without_ctx context e2 in
      If (ty, e0', e1', e2')
  | Fun (t0, t1, x, e) ->
      let e' = rec_inline_expr_without_ctx context e in
      (*todo - check this*)
      Fun (t0, t1, x, e')
  | App (ty, e0, e1) ->
      let e0' = rec_inline_expr_without_ctx (AppCtx context) e0 in
      let e1' = rec_inline_expr_without_ctx context e1 in
      App (ty, e0', e1')
  | Let (ty, x, e0, e1) -> (
      match Hashtbl.find_opt occurrence_tbl x with
      | Some Dead -> rec_inline_expr_without_ctx context e1
      | _ ->
          let e0' = rec_inline_expr_without_ctx OtherCtx e0 in
          let () = Hashtbl.add code_tbl x e0' in
          let () = Hashtbl.add size_tbl x (size_expr e0') in
          let e1' = rec_inline_expr_without_ctx context e1 in
          Let (ty, x, e0', e1'))
  | LetRec (ty, x, e0, e1) ->
      let e0' = rec_inline_expr_without_ctx OtherCtx e0 in
      let e1' = rec_inline_expr_without_ctx context e1 in
      LetRec (ty, x, e0', e1')
  | Seq (ty, es) ->
      let last, rest =
        let reversed = List.rev es in
        (List.hd reversed, List.tl reversed |> List.rev)
      in
      let rest' = List.map (rec_inline_expr_without_ctx OtherCtx) rest in
      let last' = rec_inline_expr_without_ctx context last in
      Seq (ty, rest' @ [ last' ])
  | Switch (ty, e, cases, maybe_fallback_expr) ->
      let e' = rec_inline_expr_without_ctx OtherCtx e in
      let cases' =
        List.map
          (fun (case_con, case_expr) ->
            (case_con, rec_inline_expr_without_ctx context case_expr))
          cases
      in
      let maybe_fallback_expr' =
        Option.map (rec_inline_expr_without_ctx context) maybe_fallback_expr
      in
      Switch (ty, e', cases', maybe_fallback_expr')
  | While_true e -> While_true (rec_inline_expr_without_ctx context e)
  | Break e -> Break (rec_inline_expr_without_ctx context e)
  | Assign_Seq assigns ->
      let assigns' =
        List.map
          (fun (name, ty, e) ->
            (name, ty, rec_inline_expr_without_ctx OtherCtx e))
          assigns
      in
      Assign_Seq assigns'
  | Shared_Expr (e_ref, _, seen) ->
      if !seen then e
      else (
        seen := true;
        e_ref := rec_inline_expr_without_ctx context !e_ref;
        e)
  | Tuple (ty, es) ->
      Tuple (ty, List.map (rec_inline_expr_without_ctx context) es)
  | TupleGet (ty, i, es) ->
      TupleGet (ty, i, rec_inline_expr_without_ctx context es)
  | ConstructorGet (ty, x, es) ->
      ConstructorGet (ty, x, rec_inline_expr_without_ctx context es)
  | Set_Tuple (e0, e1, e2) ->
      let e0' = rec_inline_expr_without_ctx OtherCtx e0 in
      let e1' = rec_inline_expr_without_ctx OtherCtx e1 in
      let e2' = rec_inline_expr_without_ctx OtherCtx e2 in
      Set_Tuple (e0', e1', e2')

let rec inline_decl size_tbl code_tbl occurrence_tbl most_recent_version d =
  let inlined_decl =
    match d with
    | Val (ty, x, e) ->
        let e' =
          inline_expr size_tbl code_tbl occurrence_tbl most_recent_version
            OtherCtx e
        in
        Hashtbl.add code_tbl x e';
        Hashtbl.add size_tbl x (size_expr e');
        Val (ty, x, e')
    | ValRec (ty, x, e) ->
        let e' =
          inline_expr size_tbl code_tbl occurrence_tbl most_recent_version
            OtherCtx e
        in
        (* don't bother inlining recursive expressions for now *)
        ValRec (ty, x, e')
    | Type _ -> d
    | And decls ->
        (* add code to code_tbl and size_tbl first, since mutually recursive *)
        List.iter
          (function
            | Val (_, x, e) ->
                Hashtbl.add code_tbl x e;
                Hashtbl.add size_tbl x (size_expr e)
            | _ -> ())
          decls;
        And
          (List.map
             (inline_decl size_tbl code_tbl occurrence_tbl most_recent_version)
             decls)
  in
  let () = Desugar.Utils.clear_shared_decl_seen inlined_decl in
  inlined_decl

let inline_program program =
  let size_tbl = Hashtbl.create 10 in
  let code_tbl = Hashtbl.create 10 in
  let most_recent_version = Hashtbl.create 10 in
  let occurrence_tbl = occurrence_analysis program in

  List.map
    (inline_decl size_tbl code_tbl occurrence_tbl most_recent_version)
    program
  |> List.map beta_reduce_decl
