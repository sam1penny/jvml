open Typed_ast
open Printf
open Parsing
open Common

let get_expr_type = function
  | Int _ -> TyInt
  | Ident (_, t, _) -> t
  | Bool _ -> TyBool
  | Unit _ -> TyUnit
  | Bop (_, t, _, _, _) -> t
  | If (_, t, _, _, _) -> t
  | Fun (_, t, _, _) -> t
  | App (_, t, _, _) -> t
  | Match (_, t, _, _) -> t
  | Tuple (_, t, _) -> t
  | Let (_, t, _, _, _) -> t
  | Constr (_, t, _) -> t

let get_decl_type = function Val (_, t, _, _) -> t | Type (_, t, _, _, _) -> t

module Unifications = Disjoint_set.Make (struct
  type t = type_expr

  let equal = ( = )
  let hash x = Hashtbl.hash x

  let should_be_rep x y =
    match (x, y) with
    | _, TyVar _ -> true
    | TyVar _, _ -> false
    | _, _ -> true (* arbitrary tie-breaking *)
end)

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

(* environment stuff *)
let rec tyvars_from_type = function
  | TyInt | TyBool | TyUnit -> StringSet.empty
  | TyVar v -> StringSet.singleton v
  | TyTuple ts ->
      List.fold_left
        (fun acc t -> StringSet.union (tyvars_from_type t) acc)
        StringSet.empty ts
  | TyFun (t0, t1) ->
      StringSet.union (tyvars_from_type t0) (tyvars_from_type t1)
  | TyCustom (targs, _) ->
      List.fold_left
        (fun acc t -> StringSet.union (tyvars_from_type t) acc)
        StringSet.empty targs

let free_vars_ty t bound = StringSet.diff (tyvars_from_type t) bound

let free_vars_env env =
  StringMap.fold
    (fun _ (v, bound) acc -> free_vars_ty v bound |> StringSet.union acc)
    env StringSet.empty

let generalize env ty =
  let scheme = StringSet.diff (tyvars_from_type ty) (free_vars_env env) in
  (ty, scheme)

(* end environment stuff *)

let get_pattern_type = function
  | Typed_ast.Pat_Int _ -> TyInt
  | Typed_ast.Pat_Ident (_, t, _) -> t
  | Typed_ast.Pat_Bool _ -> TyBool
  | Typed_ast.Pat_Unit _ -> TyUnit
  | Typed_ast.Pat_Any (_, t) -> t
  | Typed_ast.Pat_Or (_, t, _, _) -> t
  | Typed_ast.Pat_Tuple (_, t, _) -> t
  | _ -> raise @@ Invalid_argument "unsupported pattern!"

(* return (type, variable bindings, )*)
let rec validate_pattern nt =
  let union_maps_if_disjoint maps =
    List.fold_left
      (fun acc map ->
        acc >>=? fun acc ->
        let intersection =
          StringMap.merge
            (fun _ x y ->
              x >>= fun x ->
              y >>= fun _ -> Some x)
            acc map
        in
        if StringMap.cardinal intersection > 0 then
          Error "pattern contains duplicate bindings"
        else Ok (StringMap.union (fun _ x _ -> Some x) acc map))
      (Ok StringMap.empty) maps
  in
  let open Parsing in
  function
  | Parsed_ast.Pat_Int (loc, i) ->
      Ok (Typed_ast.Pat_Int (loc, i), StringMap.empty)
  | Parsed_ast.Pat_Ident (loc, v) ->
      let v_type = nt () in
      Ok
        ( Typed_ast.Pat_Ident (loc, v_type, v),
          StringMap.singleton v (v_type, StringSet.empty) )
  | Parsed_ast.Pat_Bool (loc, b) ->
      Ok (Typed_ast.Pat_Bool (loc, b), StringMap.empty)
  | Parsed_ast.Pat_Unit loc -> Ok (Typed_ast.Pat_Unit loc, StringMap.empty)
  | Parsed_ast.Pat_Any loc ->
      Ok (Typed_ast.Pat_Any (loc, nt ()), StringMap.empty)
      (* think about this more, but seems legit *)
  | Parsed_ast.Pat_Or (loc, p1, p2) ->
      validate_pattern nt p1 >>=? fun (p1node, p1bindings) ->
      validate_pattern nt p2 >>=? fun (p2node, p2bindings) ->
      union_maps_if_disjoint [ p1bindings; p2bindings ]
      >>=? fun updated_bindings ->
      if get_pattern_type p1node <> get_pattern_type p2node then
        Error "patterns are not of the same type"
      else
        Ok
          ( Typed_ast.Pat_Or (loc, get_pattern_type p1node, p1node, p2node),
            updated_bindings )
  | Parsed_ast.Pat_Tuple (loc, ps) ->
      List.map (validate_pattern nt) ps |> collect_result
      >>=? fun recursive_calls ->
      let pnodes = List.map (fun (x, _) -> x) recursive_calls in
      let t = TyTuple (List.map get_pattern_type pnodes) in
      union_maps_if_disjoint (List.map (fun (_, y) -> y) recursive_calls)
      >>=? fun bindings -> Ok (Typed_ast.Pat_Tuple (loc, t, pnodes), bindings)
  | _ -> raise @@ Invalid_argument "unsupported pattern!"

let rec map_over_texpr_vars f = function
  | (TyInt | TyBool | TyUnit) as ty -> ty
  | TyVar v -> f v
  | TyTuple ts -> TyTuple (List.map (map_over_texpr_vars f) ts)
  | TyFun (t0, t1) ->
      let t0' = map_over_texpr_vars f t0 in
      let t1' = map_over_texpr_vars f t1 in
      TyFun (t0', t1')
  | TyCustom (targs, tname) ->
      TyCustom (List.map (map_over_texpr_vars f) targs, tname)

let rec map_over_expr_texprs f expr =
  match expr with
  | Int _ | Bool _ | Unit _ -> expr
  | Ident (loc, ty, x) -> Ident (loc, f ty, x)
  | Bop (loc, ty, e0, op, e1) ->
      Bop (loc, f ty, map_over_expr_texprs f e0, op, map_over_expr_texprs f e1)
  | If (loc, ty, e0, e1, e2) ->
      If
        ( loc,
          f ty,
          map_over_expr_texprs f e0,
          map_over_expr_texprs f e1,
          map_over_expr_texprs f e2 )
  | Fun (loc, ty, x, e) -> Fun (loc, f ty, x, map_over_expr_texprs f e)
  | App (loc, ty, e0, e1) -> App (loc, f ty, e0, e1)
  | Match (loc, ty, e, cases) ->
      Match
        ( loc,
          f ty,
          map_over_expr_texprs f e,
          List.map (fun (p, e) -> (p, map_over_expr_texprs f e)) cases )
  | Tuple (loc, ty, es) ->
      Tuple (loc, f ty, List.map (map_over_expr_texprs f) es)
  | Let (loc, ty, x, e0, e1) ->
      Let (loc, f ty, x, map_over_expr_texprs f e0, map_over_expr_texprs f e1)
  | Constr (loc, ty, cname) -> Constr (loc, f ty, cname)

let map_over_decl_texprs f decl =
  match decl with
  | Val (loc, ty, x, e) -> Val (loc, f ty, x, map_over_expr_texprs f e)
  | Type _ -> decl (* todo: verify this is correct *)

let instantiate nt bound t =
  let remap =
    StringSet.fold
      (fun bounded_var acc -> StringMap.add bounded_var (nt ()) acc)
      bound StringMap.empty
  in
  let remap_if_bound v =
    Option.value (StringMap.find_opt v remap) ~default:(TyVar v)
  in
  map_over_texpr_vars remap_if_bound t

let occurs_in v1 t = StringSet.mem v1 (tyvars_from_type t)

(* todo - change this to use map_over_texpr_vars *)
let rec find_unified_type u ty =
  match ty with
  | TyInt | TyBool | TyUnit -> ty
  | TyVar _ ->
      let ty' = Unifications.find u ty in
      if ty = ty' then ty else find_unified_type u ty'
  | TyTuple ts -> TyTuple (List.map (find_unified_type u) ts)
  | TyFun (t1, t2) -> TyFun (find_unified_type u t1, find_unified_type u t2)
  | TyCustom (targs, tname) ->
      TyCustom (List.map (find_unified_type u) targs, tname)

let rec unify unifications t1 t2 =
  let t1 = find_unified_type unifications t1 in
  let t2 = find_unified_type unifications t2 in
  if t1 = t2 then Ok ()
  else
    match (t1, t2) with
    | TyVar v1, _ when not @@ occurs_in v1 t2 ->
        Unifications.union unifications t1 t2;
        Ok ()
    | _, TyVar v2 when not @@ occurs_in v2 t1 ->
        Unifications.union unifications t2 t1;
        Ok ()
    | TyFun (t1a, t1b), TyFun (t2a, t2b) ->
        unify unifications t1a t2a >>=? fun _ -> unify unifications t1b t2b
    | TyTuple t1s, TyTuple t2s when List.length t1s = List.length t2s ->
        List.combine t1s t2s
        |> List.map (fun (t1, t2) -> unify unifications t1 t2)
        |> collect_result
        >>=? fun _ -> Ok ()
    | TyCustom (params1, name1), TyCustom (params2, name2) when name1 = name2 ->
        List.combine params1 params2
        |> List.map (fun (p1, p2) -> unify unifications p1 p2)
        |> collect_result
        >>=? fun _ -> Ok ()
    | _ -> Error (sprintf "cannot unify %s with %s" (pp_texpr t1) (pp_texpr t2))

let make_new_type () =
  let t = ref 0 in
  fun () ->
    let n = !t in
    let _ = t := !t + 1 in
    TyVar ("t" ^ string_of_int n)

let list_all_eq = function [] -> true | x :: xs -> List.for_all (( = ) x) xs

let rec type_expr unifications nt env expr =
  match expr with
  | Parsed_ast.Int (loc, i) -> Ok (Typed_ast.Int (loc, i))
  | Parsed_ast.Bool (loc, b) -> Ok (Typed_ast.Bool (loc, b))
  | Parsed_ast.Unit loc -> Ok (Typed_ast.Unit loc)
  | Parsed_ast.Ident (loc, v) -> (
      match StringMap.find_opt v env with
      | None -> Error (sprintf "unbound variable %s" v)
      | Some (t, bound) ->
          let instantiated = instantiate nt bound t in
          Ok
            (Typed_ast.Ident
               (loc, find_unified_type unifications instantiated, v)))
  | Parsed_ast.Bop (loc, e0, op, e1) ->
      let arg_type = bop_arg_type nt op in
      type_expr unifications nt env e0 >>=? fun e0node ->
      unify unifications (get_expr_type e0node) arg_type >>=? fun _ ->
      type_expr unifications nt env e1 >>=? fun e1node ->
      unify unifications (get_expr_type e1node) arg_type >>=? fun _ ->
      Ok (Typed_ast.Bop (loc, bop_return_type op, e0node, op, e1node))
  | Parsed_ast.If (loc, e0, e1, e2) ->
      type_expr unifications nt env e0 >>=? fun e0node ->
      unify unifications (get_expr_type e0node) TyBool >>=? fun _ ->
      type_expr unifications nt env e1 >>=? fun e1node ->
      type_expr unifications nt env e2 >>=? fun e2node ->
      unify unifications (get_expr_type e1node) (get_expr_type e2node)
      >>=? fun _ ->
      Ok (Typed_ast.If (loc, get_expr_type e1node, e0node, e1node, e2node))
  | Parsed_ast.Fun (loc, x, e) ->
      let tau = nt () in
      let env' = StringMap.add x (tau, StringSet.empty) env in
      type_expr unifications nt env' e >>=? fun enode ->
      Ok (Typed_ast.Fun (loc, TyFun (tau, get_expr_type enode), x, enode))
  | Parsed_ast.App (loc, e0, e1) ->
      let tau' = nt () in
      type_expr unifications nt env e0 >>=? fun e0node ->
      type_expr unifications nt env e1 >>=? fun e1node ->
      unify unifications
        (TyFun (get_expr_type e1node, tau'))
        (get_expr_type e0node)
      >>=? fun _ -> Ok (App (loc, tau', e0node, e1node))
  (* ignore the type of the pattern at the moment *)
  | Parsed_ast.Match (loc, e, cases) ->
      type_expr unifications nt env e >>=? fun enode ->
      List.map (check_case (get_expr_type enode) unifications nt env) cases
      |> collect_result
      >>=? fun inferred_cases ->
      let first_case_expr = List.hd inferred_cases |> fun (_, e) -> e in
      let other_case_exprs =
        List.tl inferred_cases |> List.map (fun (_, e) -> e)
      in
      List.map
        (unify unifications (get_expr_type first_case_expr))
        (List.map get_expr_type other_case_exprs)
      |> collect_result
      >>=? fun _ ->
      Ok
        (Typed_ast.Match
           (loc, get_expr_type first_case_expr, enode, inferred_cases))
  | Parsed_ast.Tuple (loc, ts) ->
      List.map (type_expr unifications nt env) ts |> collect_result
      >>=? fun enodes ->
      let ty = TyTuple (List.map get_expr_type enodes) in
      Ok (Typed_ast.Tuple (loc, ty, enodes))
  | Parsed_ast.Let (loc, x, e0, e1) ->
      type_expr unifications nt env e0 >>=? fun e0node ->
      let env' =
        StringMap.add x
          (generalize env
             (find_unified_type unifications (get_expr_type e0node)))
          env
      in
      type_expr unifications nt env' e1 >>=? fun e1node ->
      Ok (Typed_ast.Let (loc, get_expr_type e1node, x, e0node, e1node))
  | Parsed_ast.Constr (loc, cname) -> (
      match StringMap.find_opt cname env with
      | None -> Error (sprintf "unbound constructor %s" cname)
      | Some (t, bound) ->
          let instantiated = instantiate nt bound t in
          Ok
            (Typed_ast.Constr
               (loc, find_unified_type unifications instantiated, cname)))

and check_case e_type unifications nt env (pattern, case_expr) =
  validate_pattern nt pattern >>=? fun (pattern_node, new_bindings) ->
  (* todo : give better error than 'cannot unify' *)
  unify unifications e_type (get_pattern_type pattern_node) >>=? fun _ ->
  let env' =
    StringMap.union
      (fun _ _ shadow_binding -> Some shadow_binding)
      env new_bindings
  in
  type_expr unifications nt env' case_expr >>=? fun case_node ->
  (*Ok (pattern_type, case_node)*)
  Ok (pattern_node, case_node)

let make_new_char () =
  let t = ref (Char.code 'a') in
  fun () ->
    let n = !t in
    let _ = t := !t + 1 in
    Printf.sprintf "'%c" (Char.chr n)

module StringTbl = Hashtbl.Make (struct
  type t = string

  let equal = String.equal
  let hash = Hashtbl.hash
end)

let simplify_texpr ty =
  let nc = make_new_char () in
  let remap = StringTbl.create 10 in
  let simplify_tvar v =
    if not @@ StringTbl.mem remap v then StringTbl.add remap v (nc ());
    TyVar (StringTbl.find remap v)
  in
  map_over_texpr_vars simplify_tvar ty

let simplify_texpr_state () =
  let nc = make_new_char () in
  let remap = StringTbl.create 10 in
  fun v ->
    if not @@ StringTbl.mem remap v then StringTbl.add remap v (nc ()) else ();
    TyVar (StringTbl.find remap v)

let type_expr_from_scratch expr =
  let u = Unifications.create 10 in
  let env = StringMap.empty in
  let type_gen = make_new_type () in
  type_expr u type_gen env expr
  |> Result.map (fun tytree ->
         find_unified_type u (get_expr_type tytree) |> simplify_texpr)

let infer_constructor _ _ env type_constructor params = function
  | Parsed_ast.DeclConstr (_, cname, None) ->
      StringMap.add cname (type_constructor, StringSet.empty) env
  | Parsed_ast.DeclConstr (_, cname, Some texpr) ->
      (* todo : check if need to validate texpr *)
      StringMap.add cname
        (TyFun (texpr, type_constructor), StringSet.of_list params)
        env

let to_typed_constr = function
  | Parsed_ast.DeclConstr (loc, name, maybe_texpr) ->
      Typed_ast.DeclConstr (loc, name, maybe_texpr)

let type_decl unifications nt env = function
  | Parsed_ast.Val (loc, v, expr) ->
      (* support recursion *)
      let v_type = nt () in
      let env' = StringMap.add v (v_type, StringSet.empty) env in
      type_expr unifications nt env' expr >>=? fun exprnode ->
      unify unifications v_type (get_expr_type exprnode) >>=? fun _ ->
      Ok (Typed_ast.Val (loc, get_expr_type exprnode, v, exprnode), env')
  | Parsed_ast.Type (loc, params, tname, constructors) ->
      let type_constructor =
        TyCustom (List.map (fun p -> TyVar p) params, tname)
      in
      let env' =
        List.fold_left
          (fun env c ->
            infer_constructor unifications nt env type_constructor params c)
          env constructors
      in
      Ok
        ( Typed_ast.Type
            ( loc,
              type_constructor,
              params,
              tname,
              List.map to_typed_constr constructors ),
          env' )

let type_decl_from_scratch decl =
  let u = Unifications.create 20 in
  let env = StringMap.empty in
  let type_gen = make_new_type () in
  type_decl u type_gen env decl >>=? fun (declnode, _) ->
  let state = simplify_texpr_state () in
  let simplify_texpr texpr =
    find_unified_type u texpr |> map_over_texpr_vars state
  in
  Ok (map_over_decl_texprs simplify_texpr declnode)

let type_program program =
  let u = Unifications.create 20 in
  let env = StringMap.empty in
  let type_gen = make_new_type () in
  List.fold_left
    (fun acc decl ->
      acc >>=? fun (declnodes, env) ->
      type_decl u type_gen env decl >>=? fun (declnode, env') ->
      Ok (declnode :: declnodes, env'))
    (Ok ([], env))
    program
  >>=? fun (reversed_ttree, _) ->
  let ttree = List.rev reversed_ttree in
  let state = simplify_texpr_state () in
  let simplify_texpr texpr =
    find_unified_type u texpr |> map_over_texpr_vars state
  in
  let simplify_decl decl =
    match decl with
    | Val (loc, texpr, x, e) -> Val (loc, simplify_texpr texpr, x, e)
    | Type (loc, texpr, params, tname, constructors) ->
        Type (loc, simplify_texpr texpr, params, tname, constructors)
  in
  Ok (List.map simplify_decl ttree)
