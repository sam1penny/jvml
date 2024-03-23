open Desugar.Desugared_ast

(* todo - refactor into common place *)
let con_index = function
  | IntCon i -> i
  | BoolCon b -> if b then 1l else 0l
  | UnitCon -> 0l
  | AdtCon (_, tag) -> tag

let expr_index = function
  | Int i -> i
  | Bool b -> if b then 1l else 0l
  | Unit -> 0l
  | _ -> raise @@ Failure "called expr_index on non const expression!"

let rec const_prop_expr const_tbl e =
  (*Desugared_ast.pp_expr e;*)
  let rec_const_prop = const_prop_expr const_tbl in
  match e with
  | Ident (_, x) -> (
      match Hashtbl.find_opt const_tbl x with None -> e | Some c -> c)
  | Let (ty, x, e0, e1) -> (
      let e0' = rec_const_prop e0 in
      match e0' with
      (* constant propagation *)
      | Int _ | Bool _ | Unit | Constr _
      (* copy propagation *)
      | Ident _ ->
          let _ = Hashtbl.add const_tbl x e0' in
          rec_const_prop e1
      | _ -> Let (ty, x, e0', rec_const_prop e1)
      (* todo - consider transforming into A-normal form to simplify constant propagation.

         Only cost is, always assigning to variables (let x = 1 in let y = 2 in x + y) has significant performance decrease.

         But maybe these disappear after optimisations?
      *))
  | If (ty, e0, e1, e2) -> (
      match rec_const_prop e0 with
      | Bool true -> rec_const_prop e1
      | Bool false -> rec_const_prop e2
      | e0' ->
          let e1' = rec_const_prop e1 in
          let e2' = rec_const_prop e2 in
          If (ty, e0', e1', e2'))
  | Switch (ty, e, cases, maybe_fallback_opt) -> (
      let e' = rec_const_prop e in
      let cases' =
        List.map (fun (con, case_expr) -> (con, rec_const_prop case_expr)) cases
      in
      let maybe_fallback_opt' = Option.map rec_const_prop maybe_fallback_opt in
      match e' with
      | Int _ | Bool _ | Unit | Constr _ -> (
          (* todo - temporary bodge until Constr carries around constructor index *)
          let maybe_matched_expr =
            match e' with
            | Int _ | Bool _ | Unit ->
                List.find_opt
                  (fun (case_con, _) -> expr_index e' = con_index case_con)
                  cases
            | Constr (_, cname) ->
                List.find_opt
                  (function
                    | AdtCon (case_cname, _), _ -> cname = case_cname
                    | _ ->
                        raise
                        @@ Failure
                             "illegal state, constructor in case expr should \
                              be an ADTCon")
                  cases
            | _ -> raise @@ Failure "illegal state"
          in
          match (maybe_matched_expr, maybe_fallback_opt) with
          | None, Some fallback_expr -> fallback_expr
          (* no fallback, will raise match_exception at runtime, nothing we can simplify *)
          | None, None -> Switch (ty, e', cases', maybe_fallback_opt')
          | Some (_, matched_expr), _ -> matched_expr)
      | _ -> Switch (ty, e', cases', maybe_fallback_opt'))
  | _ -> Desugar.Utils.map_over_sub_expr rec_const_prop e

let const_prop_decl const_tbl decl =
  let prop_decl =
    Desugar.Utils.map_over_decl_exprs (const_prop_expr const_tbl) decl
  in
  Desugar.Utils.clear_shared_decl_seen prop_decl;
  match prop_decl with
  | Val (_, x, ((Int _ | Bool _ | Unit | Constr _ | Ident _) as e)) ->
      let _ = Hashtbl.add const_tbl x e in
      prop_decl
  | _ -> prop_decl

let const_prop_program program =
  let const_tbl = Hashtbl.create 10 in
  List.map (const_prop_decl const_tbl) program
