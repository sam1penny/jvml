open Printf
open Parsing
open Common

let get_expr_type =
  let open Typed_ast in
  function
  | Int _ -> TyInt
  | Ident (_, t, _) -> t
  | Bool _ -> TyBool
  | Unit _ -> TyUnit
  | Bop (_, t, _, _, _) -> t
  | If (_, t, _, _, _) -> t
  | Fun (_, t0, t1, _, _) -> TyFun (t0, t1)
  | App (_, t, _, _) -> t
  | Match (_, t, _, _) -> t
  | Tuple (_, t, _) -> t
  | Let (_, t, _, _, _) | LetRec (_, t, _, _, _) -> t
  | Constr (_, t, _) -> t
  | Seq (_, t, _) -> t

let get_decl_type =
  let open Typed_ast in
  function Val (_, t, _, _) | ValRec (_, t, _, _) | Type (_, t, _, _, _) -> t

module Unifications = Disjoint_set.Make (struct
  type t = Typed_ast.type_expr

  let equal = ( = )
  let hash x = Hashtbl.hash x

  let should_be_rep x y =
    match (x, y) with
    | _, Typed_ast.TyVar _ -> true
    | Typed_ast.TyVar _, _ -> false
    | _, _ -> true (* arbitrary tie-breaking *)
end)

(* environment stuff *)
let rec tyvars_from_type ty =
  let open Typed_ast in
  match ty with
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
  | Typed_ast.Pat_Int _ -> Typed_ast.TyInt
  | Typed_ast.Pat_Ident (_, t, _) -> t
  | Typed_ast.Pat_Bool _ -> Typed_ast.TyBool
  | Typed_ast.Pat_Unit _ -> Typed_ast.TyUnit
  | Typed_ast.Pat_Any (_, t) -> t
  | Typed_ast.Pat_Or (_, t, _) -> t
  | Typed_ast.Pat_Tuple (_, t, _) -> t
  | Typed_ast.Pat_Constr (_, t, _, _) -> t

let rec map_over_texpr_vars f =
  let open Typed_ast in
  function
  | (TyInt | TyBool | TyUnit) as ty -> ty
  | TyVar v -> f v
  | TyTuple ts -> TyTuple (List.map (map_over_texpr_vars f) ts)
  | TyFun (t0, t1) ->
      let t0' = map_over_texpr_vars f t0 in
      let t1' = map_over_texpr_vars f t1 in
      TyFun (t0', t1')
  | TyCustom (targs, tname) ->
      TyCustom (List.map (map_over_texpr_vars f) targs, tname)

let rec map_over_pat_texprs f pat =
  let open Typed_ast in
  match pat with
  | Pat_Int _ | Pat_Bool _ | Pat_Unit _ -> pat
  | Pat_Ident (loc, ty, x) -> Pat_Ident (loc, f ty, x)
  | Pat_Any (loc, ty) -> Pat_Any (loc, f ty)
  | Pat_Or (loc, ty, pats) ->
      let ty' = f ty in
      Pat_Or (loc, ty', List.map (map_over_pat_texprs f) pats)
  | Pat_Tuple (loc, ty, pats) ->
      let ty' = f ty in
      Pat_Tuple (loc, ty', List.map (map_over_pat_texprs f) pats)
  | Pat_Constr (loc, ty, x, pat_opt) ->
      let ty' = f ty in
      Pat_Constr (loc, ty', x, Option.map (map_over_pat_texprs f) pat_opt)

let rec map_over_expr_texprs f expr =
  let open Typed_ast in
  match expr with
  | Int _ | Bool _ | Unit _ -> expr
  | Ident (loc, ty, x) -> Ident (loc, f ty, x)
  | Bop (loc, ty, e0, op, e1) ->
      let ty' = f ty in
      let e0' = map_over_expr_texprs f e0 in
      let e1' = map_over_expr_texprs f e1 in
      Bop (loc, ty', e0', op, e1')
  | If (loc, ty, e0, e1, e2) ->
      let ty' = f ty in
      let e0' = map_over_expr_texprs f e0 in
      let e1' = map_over_expr_texprs f e1 in
      let e2' = map_over_expr_texprs f e2 in
      If (loc, ty', e0', e1', e2')
  | Fun (loc, t0, t1, x, e) ->
      let t0' = f t0 in
      let t1' = f t1 in
      Fun (loc, t0', t1', x, map_over_expr_texprs f e)
  | App (loc, ty, e0, e1) ->
      let ty' = f ty in
      let e0' = map_over_expr_texprs f e0 in
      let e1' = map_over_expr_texprs f e1 in
      App (loc, ty', e0', e1')
  | Match (loc, ty, e, cases) ->
      let ty' = f ty in
      let e' = map_over_expr_texprs f e in
      Match
        ( loc,
          ty',
          e',
          List.map
            (fun (p, e) -> (map_over_pat_texprs f p, map_over_expr_texprs f e))
            cases )
  | Tuple (loc, ty, es) ->
      let ty' = f ty in
      Tuple (loc, ty', List.map (map_over_expr_texprs f) es)
  | Let (loc, ty, x, e0, e1) ->
      let ty' = f ty in
      let e0' = map_over_expr_texprs f e0 in
      let e1' = map_over_expr_texprs f e1 in
      Let (loc, ty', x, e0', e1')
  | LetRec (loc, ty, x, e0, e1) ->
      let ty' = f ty in
      let e0' = map_over_expr_texprs f e0 in
      let e1' = map_over_expr_texprs f e1 in
      LetRec (loc, ty', x, e0', e1')
  | Constr (loc, ty, cname) -> Constr (loc, f ty, cname)
  | Seq (loc, ty, es) ->
      let ty' = f ty in
      Seq (loc, ty', List.map (map_over_expr_texprs f) es)

let map_over_decl_texprs f decl =
  let open Typed_ast in
  match decl with
  | Val (loc, ty, x, e) ->
      let ty' = f ty in
      Val (loc, ty', x, map_over_expr_texprs f e)
  | ValRec (loc, ty, x, e) ->
      let ty' = f ty in
      ValRec (loc, ty', x, map_over_expr_texprs f e)
  | Type _ -> decl (* todo: verify this is correct *)

let instantiate nt bound t =
  let remap =
    StringSet.fold
      (fun bounded_var acc -> StringMap.add bounded_var (nt ()) acc)
      bound StringMap.empty
  in
  let remap_if_bound v =
    Option.value (StringMap.find_opt v remap) ~default:(Typed_ast.TyVar v)
  in
  map_over_texpr_vars remap_if_bound t

let occurs_in v1 t = StringSet.mem v1 (tyvars_from_type t)

let rec find_unified_type u ty =
  let open Typed_ast in
  map_over_texpr_vars
    (fun v ->
      let ty = TyVar v in
      let ty' = Unifications.find u ty in
      if ty = ty' then ty else find_unified_type u ty')
    ty

(** loc_on_fail should be the location of t2, although this is not enforced. *)
let rec unify unifications t1 t2 loc_on_fail =
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
        unify unifications t1a t2a loc_on_fail >>=? fun _ ->
        unify unifications t1b t2b loc_on_fail
    | TyTuple t1s, TyTuple t2s when List.length t1s = List.length t2s ->
        List.combine t1s t2s
        |> List.map (fun (t1, t2) -> unify unifications t1 t2 loc_on_fail)
        |> collect_result
        >>=? fun _ -> Ok ()
    | TyCustom (params1, name1), TyCustom (params2, name2) when name1 = name2 ->
        if List.length params1 <> List.length params2 then
          Error (loc_on_fail, "illegal state - something went wrong")
        else
          List.combine params1 params2
          |> List.map (fun (p1, p2) -> unify unifications p1 p2 loc_on_fail)
          |> collect_result
          >>=? fun _ -> Ok ()
    | _ ->
        (* Error (loc_on_fail, sprintf "cannot unify %s with %s" (pp_texpr t1) (pp_texpr t2)) *)
        Error
          ( loc_on_fail,
            sprintf
              "this expression has type %s but was expected to have type %s"
              (Typed_ast.pp_texpr t1) (Typed_ast.pp_texpr t2) )

(* return (type, variable bindings, )*)
let rec validate_pattern unifications env nt =
  (* todo :
     -- report more refine location complaints --
     to achieve this i should could pass the maps down to recursive calls,
     so that I can fail when matching a pat_ident case rather than when unioning maps
  *)
  let union_maps_if_disjoint loc maps =
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
          let first_duplicate =
            StringMap.find_first (fun _ -> true) intersection |> fun (x, _) -> x
          in
          Error
            ( loc,
              sprintf "pattern contains duplicate binding for %s"
                first_duplicate )
        else Ok (StringMap.union (fun _ x _ -> Some x) acc map))
      (Ok StringMap.empty) maps
  in
  let verify_same_bindings_same_types unifications loc maps =
    let first_map, other_maps = (List.hd maps, List.tl maps) in
    List.map
      (fun other_map ->
        StringMap.fold
          (fun x _ acc ->
            acc >>=? fun _ ->
            if StringMap.mem x other_map then Ok ()
            else
              Error
                ( loc,
                  sprintf "Variable %s must occur on both sides of the pattern"
                    x ))
          first_map (Ok ())
        >>=? fun _ ->
        StringMap.fold
          (fun x (ty2, _) acc ->
            acc >>=? fun _ ->
            match StringMap.find_opt x first_map with
            | None ->
                Error
                  ( loc,
                    sprintf
                      "Variable %s must occur on both sides of the pattern" x )
            | Some (ty1, _) -> unify unifications ty1 ty2 loc)
          other_map (Ok ()))
      other_maps
    |> collect_result
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
  | Parsed_ast.Pat_Or (loc, pats) ->
      List.map (validate_pattern unifications env nt) pats |> collect_result
      >>=? fun recursive_calls ->
      let pat_nodes = List.map (fun (x, _) -> x) recursive_calls in
      let bindings = List.map (fun (_, y) -> y) recursive_calls in
      let first_node, other_nodes = (List.hd pat_nodes, List.tl pat_nodes) in
      List.map get_pattern_type other_nodes
      |> List.map (fun ty ->
             unify unifications (get_pattern_type first_node) ty loc)
      |> collect_result
      >>=? fun _ ->
      verify_same_bindings_same_types unifications loc bindings >>=? fun _ ->
      Ok
        ( Typed_ast.Pat_Or (loc, get_pattern_type first_node, pat_nodes),
          StringMap.empty )
  | Parsed_ast.Pat_Tuple (loc, ps) ->
      List.map (validate_pattern unifications env nt) ps |> collect_result
      >>=? fun recursive_calls ->
      let pnodes = List.map (fun (x, _) -> x) recursive_calls in
      let t = Typed_ast.TyTuple (List.map get_pattern_type pnodes) in
      union_maps_if_disjoint loc (List.map (fun (_, y) -> y) recursive_calls)
      >>=? fun bindings -> Ok (Typed_ast.Pat_Tuple (loc, t, pnodes), bindings)
  | Parsed_ast.Pat_Constr (loc, cname, None) -> (
      match StringMap.find_opt cname env with
      | None -> Error (loc, sprintf "unbound constructor %s" cname)
      | Some (t, bound) ->
          let instantiated = instantiate nt bound t in
          Ok
            ( Typed_ast.Pat_Constr
                (loc, find_unified_type unifications instantiated, cname, None),
              StringMap.empty ))
  | Parsed_ast.Pat_Constr (loc, cname, Some pat) -> (
      match StringMap.find_opt cname env with
      | None -> Error (loc, sprintf "unbound constructor %s" cname)
      | Some (t, bound) -> (
          validate_pattern unifications env nt pat >>=? fun (pat, bindings) ->
          let instantiated = instantiate nt bound t in
          match instantiated with
          | TyFun (args, tname) ->
              unify unifications (get_pattern_type pat) args loc >>=? fun _ ->
              Ok (Typed_ast.Pat_Constr (loc, tname, cname, Some pat), bindings)
          | _ ->
              raise
              @@ Invalid_argument
                   "illegal state - should be impossible to get here"))

let make_new_type () =
  let t = ref 0 in
  fun () ->
    let n = !t in
    let _ = t := !t + 1 in
    Typed_ast.TyVar ("t" ^ string_of_int n)

let rec type_expr unifications nt env expr =
  match expr with
  | Parsed_ast.Int (loc, i) -> Ok (Typed_ast.Int (loc, i))
  | Parsed_ast.Bool (loc, b) -> Ok (Typed_ast.Bool (loc, b))
  | Parsed_ast.Unit loc -> Ok (Typed_ast.Unit loc)
  | Parsed_ast.Ident (loc, v) -> (
      match StringMap.find_opt v env with
      | None -> Error (loc, sprintf "unbound variable %s" v)
      | Some (t, bound) ->
          let instantiated =
            find_unified_type unifications t |> instantiate nt bound
          in
          Ok (Typed_ast.Ident (loc, instantiated, v)))
  | Parsed_ast.Bop (loc, e0, op, e1) -> (
      let arg_type = Typed_ast.bop_arg_type nt op in
      type_expr unifications nt env e0 >>=? fun e0node ->
      unify unifications (get_expr_type e0node) arg_type
        (Parsed_ast.get_expr_loc e0)
      >>=? fun _ ->
      type_expr unifications nt env e1 >>=? fun e1node ->
      unify unifications (get_expr_type e1node) arg_type
        (Parsed_ast.get_expr_loc e1)
      >>=? fun _ ->
      match op with
      | EQ ->
          if
            find_unified_type unifications arg_type |> function
            | Typed_ast.TyFun _ -> true
            | _ -> false
          then Error (loc, "Equality of functions not supported!")
          else
            Ok
              (Typed_ast.Bop
                 (loc, Typed_ast.bop_return_type op, e0node, op, e1node))
      | _ ->
          Ok
            (Typed_ast.Bop
               (loc, Typed_ast.bop_return_type op, e0node, op, e1node)))
  | Parsed_ast.If (loc, e0, e1, e2) ->
      type_expr unifications nt env e0 >>=? fun e0node ->
      unify unifications (get_expr_type e0node) TyBool
        (Parsed_ast.get_expr_loc e0)
      >>=? fun _ ->
      type_expr unifications nt env e1 >>=? fun e1node ->
      type_expr unifications nt env e2 >>=? fun e2node ->
      unify unifications (get_expr_type e1node) (get_expr_type e2node)
        (Parsed_ast.get_expr_loc e2)
      >>=? fun _ ->
      Ok (Typed_ast.If (loc, get_expr_type e1node, e0node, e1node, e2node))
  | Parsed_ast.Fun (loc, x, e) ->
      let tau = nt () in
      let env' = StringMap.add x (tau, StringSet.empty) env in
      type_expr unifications nt env' e >>=? fun enode ->
      Ok (Typed_ast.Fun (loc, tau, get_expr_type enode, x, enode))
  | Parsed_ast.App (loc, e0, e1) ->
      let tau' = nt () in
      type_expr unifications nt env e0 >>=? fun e0node ->
      type_expr unifications nt env e1 >>=? fun e1node ->
      unify unifications
        (TyFun (get_expr_type e1node, tau'))
        (get_expr_type e0node)
        (Parsed_ast.get_expr_loc e1)
      >>=? fun _ -> Ok (Typed_ast.App (loc, tau', e0node, e1node))
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
        (fun ty -> unify unifications (get_expr_type first_case_expr) ty loc)
        (List.map get_expr_type other_case_exprs)
      |> collect_result
      >>=? fun _ ->
      Ok
        (Typed_ast.Match
           (loc, get_expr_type first_case_expr, enode, inferred_cases))
  | Parsed_ast.Tuple (loc, ts) ->
      List.map (type_expr unifications nt env) ts |> collect_result
      >>=? fun enodes ->
      let ty = Typed_ast.TyTuple (List.map get_expr_type enodes) in
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
  | Parsed_ast.LetRec (loc, x, e0, e1) ->
      let x_type = nt () in
      let env' = StringMap.add x (x_type, StringSet.empty) env in
      type_expr unifications nt env' e0 >>=? fun e0node ->
      let env' =
        StringMap.add x (generalize env' (get_expr_type e0node)) env'
      in
      unify unifications x_type (get_expr_type e0node) loc >>=? fun _ ->
      type_expr unifications nt env' e1 >>=? fun e1node ->
      Ok (Typed_ast.LetRec (loc, get_expr_type e1node, x, e0node, e1node))
  | Parsed_ast.Constr (loc, cname) -> (
      match StringMap.find_opt cname env with
      | None -> Error (loc, sprintf "unbound constructor %s" cname)
      | Some (t, bound) ->
          let instantiated = instantiate nt bound t in
          Ok
            (Typed_ast.Constr
               (loc, find_unified_type unifications instantiated, cname)))
  | Parsed_ast.Seq (loc, es) ->
      List.map (type_expr unifications nt env) es |> collect_result
      >>=? fun enodes ->
      let ty = List.length enodes - 1 |> List.nth enodes |> get_expr_type in
      Ok (Typed_ast.Seq (loc, ty, enodes))

and check_case e_type unifications nt env (pattern, case_expr) =
  validate_pattern unifications env nt pattern
  >>=? fun (pattern_node, new_bindings) ->
  (* todo : give better error than 'cannot unify' *)
  unify unifications e_type
    (get_pattern_type pattern_node)
    (Parsed_ast.get_pattern_loc pattern)
  >>=? fun _ ->
  let env' =
    StringMap.union
      (fun _ _ shadow_binding -> Some shadow_binding)
      env new_bindings
  in
  type_expr unifications nt env' case_expr >>=? fun case_node ->
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

let simplify_texpr_state () =
  let nc = make_new_char () in
  let remap = StringTbl.create 10 in
  fun v ->
    if not @@ StringTbl.mem remap v then StringTbl.add remap v (nc ()) else ();
    Typed_ast.TyVar (StringTbl.find remap v)

let external_env =
  let open Typed_ast in
  let alpha_list = TyCustom ([ TyVar "'a" ], "list$") in
  StringMap.of_list
    [
      ("print", (TyFun (TyVar "'a", TyUnit), StringSet.singleton "'a"));
      ("Nil$", (alpha_list, StringSet.singleton "'a"));
      ( "Cons$",
        ( TyFun (TyTuple [ TyVar "'a"; alpha_list ], alpha_list),
          StringSet.singleton "'a" ) );
    ]

let type_expr_from_scratch expr =
  let u = Unifications.create 10 in
  let env = external_env in
  let type_gen = make_new_type () in
  type_expr u type_gen env expr >>=? fun exprnode ->
  let state = simplify_texpr_state () in
  let simplify_texpr texpr =
    find_unified_type u texpr |> map_over_texpr_vars state
  in
  Ok (map_over_expr_texprs simplify_texpr exprnode)

let rec validate_texpr type_env params = function
  | Parsed_ast.TyInt _ -> Ok Typed_ast.TyInt
  | Parsed_ast.TyBool _ -> Ok Typed_ast.TyBool
  | Parsed_ast.TyUnit _ -> Ok Typed_ast.TyUnit
  | Parsed_ast.TyVar (loc, p) ->
      if List.mem p params then Ok (Typed_ast.TyVar p)
      else Error (loc, sprintf "Unbound type variable '%s" p)
  | Parsed_ast.TyCustom (loc, parameterised_by, tname) -> (
      List.map (validate_texpr type_env params) parameterised_by
      |> collect_result
      >>=? fun validated_parameterised_by ->
      match StringMap.find_opt tname type_env with
      | None -> Error (loc, sprintf "Unknown type constructor %s" tname)
      | Some n ->
          let expected_num_args = List.length parameterised_by in
          if n = expected_num_args then
            Ok (Typed_ast.TyCustom (validated_parameterised_by, tname))
          else
            Error
              ( loc,
                sprintf
                  "This type constructor expected %i argument(s), but received \
                   %i argument(s)"
                  expected_num_args n ))
  | Parsed_ast.TyTuple (_, ts) ->
      List.map (validate_texpr type_env params) ts |> collect_result
      >>=? fun ts' -> Ok (Typed_ast.TyTuple ts')
  | Parsed_ast.TyFun (_, t0, t1) ->
      validate_texpr type_env params t0 >>=? fun t0' ->
      validate_texpr type_env params t1 >>=? fun t1' ->
      Ok (Typed_ast.TyFun (t0', t1'))

let infer_constructor env type_env type_constructor params = function
  | Parsed_ast.DeclConstr (loc, cname, None) ->
      if StringMap.mem cname env then
        Error (loc, sprintf "Duplicate definition of constructor %s" cname)
      else
        Ok
          ( Typed_ast.DeclConstr (loc, cname, None),
            StringMap.add cname (type_constructor, StringSet.of_list params) env
          )
  | Parsed_ast.DeclConstr (loc, cname, Some texpr) ->
      if StringMap.mem cname env then
        Error (loc, sprintf "Duplicate definition of constructor %s" cname)
      else
        validate_texpr type_env params texpr >>=? fun validated_texpr ->
        Ok
          ( Typed_ast.DeclConstr (loc, cname, Some validated_texpr),
            StringMap.add cname
              ( Typed_ast.TyFun (validated_texpr, type_constructor),
                StringSet.of_list params )
              env )

let list_contains_distinct_elements l =
  StringSet.cardinal (StringSet.of_list l) = List.length l

let type_decl unifications nt env type_env = function
  | Parsed_ast.Val (loc, v, expr) ->
      if StringMap.mem v env then
        Error (loc, sprintf "Duplicate definition of binding %s" v)
      else
        type_expr unifications nt env expr >>=? fun exprnode ->
        let exprnode =
          map_over_expr_texprs (find_unified_type unifications) exprnode
        in
        let env' =
          StringMap.add v (generalize env (get_expr_type exprnode)) env
        in
        Ok
          ( Typed_ast.Val (loc, get_expr_type exprnode, v, exprnode),
            env',
            type_env )
  | Parsed_ast.ValRec (loc, v, expr) ->
      if StringMap.mem v env then
        Error (loc, sprintf "Duplicate definition of binding %s" v)
      else
        (* support recursion *)
        let v_type = nt () in
        let env' = StringMap.add v (v_type, StringSet.empty) env in
        type_expr unifications nt env' expr >>=? fun exprnode ->
        let exprnode =
          map_over_expr_texprs (find_unified_type unifications) exprnode
        in
        let env' =
          StringMap.add v (generalize env (get_expr_type exprnode)) env'
        in
        unify unifications v_type (get_expr_type exprnode) loc >>=? fun _ ->
        Ok
          ( Typed_ast.ValRec (loc, get_expr_type exprnode, v, exprnode),
            env',
            type_env )
  | Parsed_ast.Type (loc, params, tname, constructors) ->
      if StringMap.mem tname type_env then
        Error (loc, sprintf "Duplicate definition of type %s" tname)
      else if not @@ list_contains_distinct_elements params then
        Error (loc, "A type parameter occurs several times")
      else
        let type_constructor =
          Typed_ast.TyCustom
            (List.map (fun p -> Typed_ast.TyVar p) params, tname)
        in
        let type_env' = StringMap.add tname (List.length params) type_env in
        let acc =
          List.fold_right
            (fun c acc ->
              acc >>=? fun (typed_cs, env) ->
              infer_constructor env type_env' type_constructor params c
              >>=? fun (typed_c, env') -> Ok (typed_c :: typed_cs, env'))
            constructors
            (Ok ([], env))
        in
        acc >>=? fun (typed_constructors, env') ->
        Ok
          ( Typed_ast.Type
              (loc, type_constructor, params, tname, typed_constructors),
            env',
            type_env' )

let type_decl_from_scratch decl =
  let u = Unifications.create 20 in
  let env = external_env in
  let type_env = StringMap.empty in
  let type_gen = make_new_type () in
  type_decl u type_gen env type_env decl >>=? fun (declnode, _, _) ->
  let state = simplify_texpr_state () in
  let simplify_texpr texpr =
    find_unified_type u texpr |> map_over_texpr_vars state
  in
  Ok (map_over_decl_texprs simplify_texpr declnode)

let type_program program =
  let u = Unifications.create 20 in
  let env = external_env in
  let type_env = StringMap.empty in
  let type_gen = make_new_type () in
  List.fold_left
    (fun acc decl ->
      acc >>=? fun (declnodes, env, type_env) ->
      type_decl u type_gen env type_env decl
      >>=? fun (declnode, env', type_env') ->
      Ok (declnode :: declnodes, env', type_env'))
    (Ok ([], env, type_env))
    program
  >>=? fun (reversed_tdecls, _, _) ->
  let tdecls = List.rev reversed_tdecls in
  let remapped =
    List.map
      (fun tdecl ->
        let state = simplify_texpr_state () in
        let simplify_texpr texpr =
          find_unified_type u texpr |> map_over_texpr_vars state
        in
        map_over_decl_texprs simplify_texpr tdecl)
      tdecls
  in
  Ok remapped

let type_program_exn input program =
  match type_program program with
  | Ok typed_program -> typed_program
  | Error (loc, message) ->
      let to_internal_loc (x, y) =
        (Pp_loc.Position.of_lexing x, Pp_loc.Position.of_lexing y)
      in
      Pp_loc.pp ~input Format.std_formatter [ to_internal_loc loc ];
      (* todo install exception printers and raise exception *)
      Format.printf "Error: %s\n" message;
      Format.print_flush ();
      raise (Failure "error in type inference")

let type_program_exn_from_file filename program =
  type_program_exn (Pp_loc.Input.file filename) program

let type_program_exn_from_string string program =
  type_program_exn (Pp_loc.Input.string string) program
