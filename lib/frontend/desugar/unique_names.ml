open Desugared_ast

(*
replace all names in the program to be unique.
add a suffix for how many times we have seen the name already.
e.g:
let x = 1 in
let x = 2 in
let y = 3
-->
let x_$0 = 1 in
let x_$1 = 2 in
let y_$0 = 3

$ used for name mangling purposes.

iteration order enforced with let to
ensure readable output

DOES NOT APPLY TO CONSTRUCTORS (AT THE MOMENT)
*)

let fetch_next_version_and_update table key =
  let next_version_num =
    Hashtbl.find_opt table key
    |> Option.map (( + ) 1)
    |> Option.value ~default:0
  in
  let _ = Hashtbl.add table key next_version_num in
  key ^ "_$" ^ string_of_int next_version_num

let fetch_current_version table key =
  Hashtbl.find_opt table key |> Option.value ~default:0
  |> fun next_version_num -> key ^ "_$" ^ string_of_int next_version_num

let restore_previous_version table key = Hashtbl.remove table key

let rec rename_expr (most_recent_version : (string, int) Hashtbl.t) e =
  let rec_rename_expr = rename_expr most_recent_version in
  match e with
  | Int _ | Bool _ | Unit | Match_Failure -> e
  | Ident (ty, var) -> Ident (ty, fetch_current_version most_recent_version var)
  | Bop (ty, e0, bop, e1) ->
      let e0' = rec_rename_expr e0 in
      let e1' = rec_rename_expr e1 in
      Bop (ty, e0', bop, e1')
  | If (ty, e0, e1, e2) ->
      let e0' = rec_rename_expr e0 in
      let e1' = rec_rename_expr e1 in
      let e2' = rec_rename_expr e2 in
      If (ty, e0', e1', e2')
  | Fun (t0, t1, x, e) ->
      let x_versioned = fetch_next_version_and_update most_recent_version x in
      let e' = rec_rename_expr e in
      restore_previous_version most_recent_version x;
      Fun (t0, t1, x_versioned, e')
  | App (ty, e0, e1) ->
      let e0' = rec_rename_expr e0 in
      let e1' = rec_rename_expr e1 in
      App (ty, e0', e1')
  | Direct_app _ ->
      raise @@ Failure "invalid ordering - run Unique_names before Direct_calls"
  | Tuple (ty, es) -> Tuple (ty, List.map rec_rename_expr es)
  | Let (ty, x, e0, e1) ->
      let e0' = rec_rename_expr e0 in
      let x_versioned = fetch_next_version_and_update most_recent_version x in
      let e1' = rec_rename_expr e1 in
      restore_previous_version most_recent_version x;
      Let (ty, x_versioned, e0', e1')
  | LetRec (ty, x, e0, e1) ->
      let x_versioned = fetch_next_version_and_update most_recent_version x in
      let e0' = rec_rename_expr e0 in
      let e1' = rec_rename_expr e1 in
      restore_previous_version most_recent_version x;
      LetRec (ty, x_versioned, e0', e1')
  | Constr (ty, cname) -> Constr (ty, cname)
  | Seq (ty, es) -> Seq (ty, List.map rec_rename_expr es)
  | TupleGet (ty, i, e) -> TupleGet (ty, i, rec_rename_expr e)
  | ConstructorGet (ty, c, e) -> ConstructorGet (ty, c, rec_rename_expr e)
  | Switch (ty, e0, cases, maybe_fallback_expr) ->
      let e0' = rec_rename_expr e0 in
      let cases' =
        List.map
          (fun (con, case_expr) -> (con, rec_rename_expr case_expr))
          cases
      in
      Switch (ty, e0', cases', Option.map rec_rename_expr maybe_fallback_expr)
  | Shared_Expr (expr_ref, label_opt) ->
      (* todo - add general bool seen flag to avoid unnecessary recomputation *)
      expr_ref := rec_rename_expr !expr_ref;
      Shared_Expr (expr_ref, label_opt)
  | While_true _ | Return _ | Assign_Seq _ ->
      raise
      @@ Failure "tail rec constructs should not be present in lambda_lift"

let rename_decl most_recent_version d =
  match d with
  | Val (ty, x, e) ->
      let e' = rename_expr most_recent_version e in
      let x_versioned = fetch_next_version_and_update most_recent_version x in
      Val (ty, x_versioned, e')
  | ValRec (ty, x, e) ->
      let x_versioned = fetch_next_version_and_update most_recent_version x in
      ValRec (ty, x_versioned, rename_expr most_recent_version e)
  | Type (ty, params, tname, type_constructors) ->
      Type (ty, params, tname, type_constructors)

let rename_program program =
  let renamings = Hashtbl.create 10 in
  List.fold_left
    (fun renamed_program_rev decl ->
      rename_decl renamings decl :: renamed_program_rev)
    [] program
  |> List.rev
