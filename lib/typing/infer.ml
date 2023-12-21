open Typed_ast
open Printf
open Parsing

let ( >>=? ) x f = Result.bind x f
let ( >>= ) x f = Option.bind x f

let get_type = function
  | Int _ -> TyInt
  | Ident (t, _) -> t
  | Bool _ -> TyBool
  | Unit -> TyUnit
  | Oper (t, _, _, _) -> t
  | If (t, _, _, _) -> t
  | Fun (t, _, _) -> t
  | App (t, _, _) -> t
  | Match (t, _, _) -> t
  | Tuple (t, _) -> t
  | Let (t, _, _, _) -> t

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

let free_vars_ty t bound = StringSet.diff (tyvars_from_type t) bound

let free_vars_env env =
  StringMap.fold
    (fun _ (v, bound) acc -> free_vars_ty v bound |> StringSet.union acc)
    env StringSet.empty

let generalize env ty =
  let scheme = StringSet.diff (tyvars_from_type ty) (free_vars_env env) in
  (ty, scheme)

(* end environment stuff *)

let instantiate nt bound t =
  let remap =
    StringSet.fold
      (fun bounded_var acc -> StringMap.add bounded_var (nt ()) acc)
      bound StringMap.empty
  in
  let rec map_over = function
    | (TyInt | TyBool | TyUnit) as ty -> ty
    | TyVar v -> Option.value (StringMap.find_opt v remap) ~default:(TyVar v)
    | TyTuple ts -> TyTuple (List.map map_over ts)
    | TyFun (t0, t1) -> TyFun (map_over t0, map_over t1)
  in
  map_over t

let occurs_in v1 t = StringSet.mem v1 (tyvars_from_type t)

let rec find_unified_type u ty =
  match ty with
  | TyInt | TyBool | TyUnit -> ty
  | TyVar _ ->
      let ty' = Unifications.find u ty in
      if ty = ty' then ty else find_unified_type u ty'
  | TyTuple ts -> TyTuple (List.map (find_unified_type u) ts)
  | TyFun (t1, t2) -> TyFun (find_unified_type u t1, find_unified_type u t2)

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
    | _ -> Error (sprintf "cannot unify %s with %s" (ty_repr t1) (ty_repr t2))

let make_new_type () =
  let t = ref 0 in
  fun () ->
    let n = !t in
    let _ = t := !t + 1 in
    TyVar ("t" ^ string_of_int n)

let rec infer unifications nt env expr =
  match expr with
  | Parsed_ast.Int i -> Ok (Typed_ast.Int i)
  | Parsed_ast.Bool b -> Ok (Typed_ast.Bool b)
  | Parsed_ast.Unit -> Ok Typed_ast.Unit
  | Parsed_ast.Ident v -> (
      match StringMap.find_opt v env with
      | None -> Error (sprintf "unbound variable %s" v)
      | Some (t, bound) ->
          let instantiated = instantiate nt bound t in
          Ok (Typed_ast.Ident (find_unified_type unifications instantiated, v)))
  | Parsed_ast.Oper (e0, op, e1) ->
      infer unifications nt env e0 >>=? fun e0node ->
      unify unifications (get_type e0node) (oper_arg_type op) >>=? fun _ ->
      infer unifications nt env e1 >>=? fun e1node ->
      unify unifications (get_type e1node) (oper_arg_type op) >>=? fun _ ->
      Ok (Typed_ast.Oper (oper_return_type op, e0node, op, e1node))
  | Parsed_ast.If (e0, e1, e2) ->
      infer unifications nt env e0 >>=? fun e0node ->
      unify unifications (get_type e0node) TyBool >>=? fun _ ->
      infer unifications nt env e1 >>=? fun e1node ->
      infer unifications nt env e2 >>=? fun e2node ->
      unify unifications (get_type e1node) (get_type e2node) >>=? fun _ ->
      Ok (Typed_ast.If (get_type e1node, e0node, e1node, e2node))
  | Parsed_ast.Fun (x, e) ->
      let tau = nt () in
      let env' = StringMap.add x (tau, StringSet.empty) env in
      infer unifications nt env' e >>=? fun enode ->
      Ok (Typed_ast.Fun (TyFun (tau, get_type enode), x, enode))
  | Parsed_ast.App (e0, e1) ->
      let tau' = nt () in
      infer unifications nt env e0 >>=? fun e0node ->
      infer unifications nt env e1 >>=? fun e1node ->
      unify unifications (TyFun (get_type e1node, tau')) (get_type e0node)
      >>=? fun _ -> Ok (App (tau', e0node, e1node))
  (* ignore the type of the pattern at the moment *)
  | Parsed_ast.Match (_, _) -> raise @@ Invalid_argument "todo"
  | Parsed_ast.Tuple ts ->
      List.fold_right
        (fun e acc ->
          acc >>=? fun acc' ->
          infer unifications nt env e >>=? fun e' -> Ok (e' :: acc'))
        ts (Ok [])
      >>=? fun enodes ->
      let ty = TyTuple (List.map get_type enodes) in
      Ok (Typed_ast.Tuple (ty, enodes))
  | Parsed_ast.Let (x, e0, e1) ->
      infer unifications nt env e0 >>=? fun e0node ->
      let env' =
        StringMap.add x
          (generalize env (find_unified_type unifications (get_type e0node)))
          env
      in
      infer unifications nt env' e1 >>=? fun e1node ->
      Ok (Typed_ast.Let (get_type e1node, x, e0node, e1node))
  | _ -> Error (sprintf "current unsupported expression")

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

let simplify_types ty =
  let nc = make_new_char () in
  let remap = StringTbl.create 10 in
  let rec map_over = function
    | (TyInt | TyBool | TyUnit) as ty -> ty
    | TyVar v ->
        if not @@ StringTbl.mem remap v then StringTbl.add remap v (nc ());
        TyVar (StringTbl.find remap v)
    | TyTuple ts -> TyTuple (List.map map_over ts)
    | TyFun (t0, t1) ->
        let t0' = map_over t0 in
        let t1' = map_over t1 in
        TyFun (t0', t1')
  in
  map_over ty

let infer_expr expr =
  let u = Unifications.create 10 in
  let env = StringMap.empty in
  let type_gen = make_new_type () in
  infer u type_gen env expr
  |> Result.map (fun tytree ->
         find_unified_type u (get_type tytree) |> simplify_types |> ty_repr)

let e =
  let open Parsed_ast in
  Fun ("x", Fun ("y", Tuple [ App (Ident "x", Ident "y"); Int 3 ]))

let () =
  (infer_expr e |> function
   | Ok s -> print_endline s
   | Error e -> print_endline e);
  print_newline ()

let dothing x = x
