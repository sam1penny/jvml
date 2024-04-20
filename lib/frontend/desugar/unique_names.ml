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

let rec rename_expr (most_recent_version : (string, int) Hashtbl.t)
    (most_local_version : (string, string) Hashtbl.t) e =
  let rec_rename_expr = rename_expr most_recent_version most_local_version in
  match e with
  | Ident (ty, var) ->
      if var = "print" then Ident (ty, "print_$0")
      else Ident (ty, Hashtbl.find most_local_version var)
  | Fun (t0, t1, x, e) ->
      let x_versioned = fetch_next_version_and_update most_recent_version x in
      let _ = Hashtbl.add most_local_version x x_versioned in
      let e' = rec_rename_expr e in
      let () = restore_previous_version most_local_version x in
      Fun (t0, t1, x_versioned, e')
  | Let (ty, x, e0, e1) ->
      let e0' = rec_rename_expr e0 in
      let x_versioned = fetch_next_version_and_update most_recent_version x in
      let _ = Hashtbl.add most_local_version x x_versioned in
      let e1' = rec_rename_expr e1 in
      let () = restore_previous_version most_local_version x in
      Let (ty, x_versioned, e0', e1')
  | LetRec (ty, x, e0, e1) ->
      let x_versioned = fetch_next_version_and_update most_recent_version x in
      let _ = Hashtbl.add most_local_version x x_versioned in
      let e0' = rec_rename_expr e0 in
      let e1' = rec_rename_expr e1 in
      let () = restore_previous_version most_local_version x in
      LetRec (ty, x_versioned, e0', e1')
  | While_true _ | Break _ | Assign_Seq _ ->
      raise
      @@ Failure "tail rec constructs should not be present in lambda_lift"
  | _ -> Utils.map_over_sub_expr rec_rename_expr e

let rec rename_decl most_recent_version most_local_version d =
  match d with
  | Val (ty, x, e) ->
      let e' = rename_expr most_recent_version most_local_version e in
      let x_versioned = fetch_next_version_and_update most_recent_version x in
      let _ = Hashtbl.add most_local_version x x_versioned in
      Val (ty, x_versioned, e')
  | ValRec (ty, x, e) ->
      let x_versioned = fetch_next_version_and_update most_recent_version x in
      let _ = Hashtbl.add most_local_version x x_versioned in
      ValRec
        (ty, x_versioned, rename_expr most_recent_version most_local_version e)
  | Type (ty, params, tname, value_constructors) ->
      Type (ty, params, tname, value_constructors)
  | And decls ->
      And (List.map (rename_decl most_recent_version most_local_version) decls)

let rename_program program =
  let renamings = Hashtbl.create 10 in
  let local_renamings = Hashtbl.create 10 in
  let renamed_program =
    List.fold_left
      (fun renamed_program_rev decl ->
        rename_decl renamings local_renamings decl :: renamed_program_rev)
      [] program
    |> List.rev
  in
  Utils.clear_shared_program_seen renamed_program;
  renamed_program
