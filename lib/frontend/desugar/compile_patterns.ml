open Desugared_ast
open Typing
open Common

type clause =
  | Clause of (Desugared_ast.expr * Typed_ast.pattern) list * Desugared_ast.expr

let pp_clause (Clause (pats, body)) =
  print_newline ();
  print_endline "begin clause";
  List.iter
    (fun (e, p) ->
      pp_expr e;
      Typed_ast.pp_pattern p;
      print_endline "")
    pats;
  pp_expr body;
  print_endline "end clause";
  print_newline ()

module ConSet = Set.Make (struct
  type t = con

  let compare = compare
end)

type shared_expr = { expr : expr; ref_count : int ref }

let make_shared table expr =
  match Hashtbl.find_opt table expr with
  | None ->
      let shared = Shared_Expr (ref expr, ref None, ref false) in
      Hashtbl.add table expr { expr = shared; ref_count = ref 0 };
      shared
  | Some { expr = e; ref_count = rc } ->
      rc := !rc + 1;
      e

let clause_of_case e (pattern, case_expr) = Clause ([ (e, pattern) ], case_expr)

let get_tname = function
  | Typed_ast.TyCustom (_, tname) -> tname
  | _ -> raise @@ Failure "called get_tname on type_expr that is not TyCustom"

let expand_or_patterns clauses =
  let expand_first_or_pattern (Clause (pats, body)) =
    match
      List.find_index
        (function _, Typed_ast.Pat_Or _ -> true | _ -> false)
        pats
    with
    | None -> (false, [ Clause (pats, body) ])
    | Some i ->
        let before_pats = take pats i in
        let after_pats = drop pats i in
        let or_getter, or_pats =
          List.nth pats i |> function
          | e, Typed_ast.Pat_Or (_, _, pats) -> (e, pats)
          | _ -> raise @@ Failure "illegal state"
        in
        List.fold_left
          (fun clauses pat ->
            Clause (before_pats @ ((or_getter, pat) :: after_pats), body)
            :: clauses)
          [] or_pats
        |> List.rev
        |> fun new_clauses -> (true, new_clauses)
  in
  let rec expand_until_done clauses =
    let r = List.map expand_first_or_pattern clauses in
    List.fold_left
      (fun acc (expansion_happened, new_clauses) ->
        if expansion_happened then expand_until_done new_clauses :: acc
        else new_clauses :: acc)
      [] r
    |> List.rev |> List.flatten
  in

  List.map (fun clause -> expand_until_done [ clause ]) clauses |> List.flatten

let move_variable_patterns (Clause (pats, body)) =
  let bindings =
    List.filter_map
      (function
        | access, Typed_ast.Pat_Ident (_, ty, x) -> Some (access, (ty, x))
        | _ -> None)
      pats
  in
  let pats' =
    List.filter (function _, Typed_ast.Pat_Ident _ -> false | _ -> true) pats
  in
  let body' =
    List.fold_right
      (fun (access, (ty, x)) body -> Let (ty, x, access, body))
      bindings body
  in
  Clause (pats', body')

let move_wildcard_patterns (Clause (pats, body)) =
  let pats' =
    List.filter (function _, Typed_ast.Pat_Any _ -> false | _ -> true) pats
  in
  Clause (pats', body)

(*
todos:
- Report warning on missing/redundant cases
- Add better logic for binding variables to avoid redundant tuple_get/constructor_get
- Consider refactoring to inner methods to avoid passing (constructors & return_type) to every method
*)
let rec compile_match (table : (expr, shared_expr) Hashtbl.t)
    (constructors : con StringMap.t StringMap.t)
    (return_type : Typed_ast.type_expr) (clauses : clause list) : expr =
  let clauses = expand_or_patterns clauses in
  let clauses = List.map move_variable_patterns clauses in
  let clauses = List.map move_wildcard_patterns clauses in
  match clauses with
  | [] -> Match_Failure
  | Clause ([], body) :: _ -> make_shared table body
  | Clause (pats, _) :: _ -> (
      let branch_var = List.hd pats |> fun (x, _) -> x in
      match List.assoc branch_var pats with
      | Typed_ast.Pat_Ident _ ->
          raise
          @@ Failure
               "illegal state - move_variable_patterns should have moved all \
                variables inside"
      | Typed_ast.Pat_Any _ ->
          raise
          @@ Failure
               "illegal state - move_variable_patterns should have moved all \
                variables inside"
      | Typed_ast.Pat_Constr (_, ty, _, _) ->
          compile_constructor_match table constructors clauses branch_var
            (StringMap.find (get_tname ty) constructors)
            return_type
      | Typed_ast.Pat_Int _ ->
          compile_int_match table constructors clauses branch_var return_type
      | Typed_ast.Pat_Tuple _ ->
          compile_tuple_match table constructors return_type clauses branch_var
      | Typed_ast.Pat_Bool _ ->
          let boolcons =
            StringMap.of_list
              [ ("true", BoolCon true); ("false", BoolCon false) ]
          in
          compile_constructor_match table constructors clauses branch_var
            boolcons return_type
      | Typed_ast.Pat_Unit _ ->
          let unitcons = StringMap.singleton "unit" UnitCon in
          compile_constructor_match table constructors clauses branch_var
            unitcons return_type
      | _ -> raise @@ Failure "illegal pattern")

and compile_constructor_match table constructors (clauses : clause list)
    (branch_var : expr) (constructors_by_name : con StringMap.t)
    (return_type : Typed_ast.type_expr) =
  List.fold_right
    (fun (Clause (pats, body)) (case_clauses, fallback_clauses) ->
      let add_case_clause cname nested_match_opt =
        let con = StringMap.find cname constructors_by_name in
        let existing_clauses_for_case =
          List.assoc_opt con case_clauses |> Option.value ~default:[]
        in
        let maybe_extra_test =
          Option.map
            (fun nested_match ->
              ( ConstructorGet
                  (Infer.get_pattern_type nested_match, cname, branch_var),
                nested_match ))
            nested_match_opt
          |> Option.to_list
        in
        let new_case_clauses =
          ( con,
            Clause (List.remove_assoc branch_var pats @ maybe_extra_test, body)
            :: existing_clauses_for_case )
          :: List.remove_assoc con case_clauses
        in
        new_case_clauses
      in
      match List.assoc_opt branch_var pats with
      | None -> (case_clauses, Clause (pats, body) :: fallback_clauses)
      | Some (Typed_ast.Pat_Constr (_, _, cname, parg_opt)) ->
          (add_case_clause cname parg_opt, fallback_clauses)
      | Some (Typed_ast.Pat_Bool (_, b)) ->
          (add_case_clause (string_of_bool b) None, fallback_clauses)
      | Some (Typed_ast.Pat_Unit _) ->
          (add_case_clause "unit" None, fallback_clauses)
      | _ ->
          raise
          @@ Failure
               "illegal state - all cells in a column must be constructors")
    clauses ([], [])
  |> fun (case_clauses, fallback_clauses) ->
  let compiled_cases =
    List.map
      (fun (con, nested_match) ->
        ( con,
          compile_match table constructors return_type
            (nested_match @ fallback_clauses) ))
      case_clauses
  in
  let constructors_in_switch =
    List.map (fun (con, _) -> con) case_clauses |> ConSet.of_list
  in
  let all_constructors =
    StringMap.to_list constructors_by_name
    |> List.map (fun (_, con) -> con)
    |> ConSet.of_list
  in
  let fallback_clauses_opt =
    if ConSet.equal constructors_in_switch all_constructors then None
    else Some (compile_match table constructors return_type fallback_clauses)
  in
  Switch (return_type, branch_var, compiled_cases, fallback_clauses_opt)

and compile_int_match table constructors (clauses : clause list)
    (branch_var : expr) (return_type : Typed_ast.type_expr) =
  List.fold_right
    (fun (Clause (pats, body)) (case_clauses, fallback_clauses) ->
      match List.assoc_opt branch_var pats with
      | None -> (case_clauses, Clause (pats, body) :: fallback_clauses)
      | Some (Typed_ast.Pat_Int (_, i)) ->
          let existing_clauses_for_case =
            List.assoc_opt (IntCon i) case_clauses |> Option.value ~default:[]
          in
          let new_case_clauses =
            ( IntCon i,
              Clause (List.remove_assoc branch_var pats, body)
              :: existing_clauses_for_case )
            :: List.remove_assoc (IntCon i) case_clauses
          in
          (new_case_clauses, fallback_clauses)
      | _ ->
          raise
          @@ Failure
               "illegal state - all cells in a column must be of the same \
                constructor")
    clauses ([], [])
  |> fun (case_clauses, fallback_clauses) ->
  let compiled_cases =
    List.map
      (fun (con, nested_match) ->
        ( con,
          compile_match table constructors return_type
            (nested_match @ fallback_clauses) ))
      case_clauses
  in
  Switch
    ( return_type,
      branch_var,
      compiled_cases,
      Some (compile_match table constructors return_type fallback_clauses) )

and compile_tuple_match table constructors return_type (clauses : clause list)
    (branch_var : expr) =
  List.map
    (fun (Clause (pats, body)) ->
      match List.assoc_opt branch_var pats with
      | None -> Clause (pats, body)
      | Some (Typed_ast.Pat_Tuple (_, _, nested_pats)) ->
          let vars =
            List.mapi
              (fun i nested_pat ->
                TupleGet (Infer.get_pattern_type nested_pat, i, branch_var))
              nested_pats
          in
          Clause
            ( List.remove_assoc branch_var pats @ List.combine vars nested_pats,
              body )
      | _ ->
          raise
          @@ Failure
               "illegal state - all cells in a column must be of the same \
                constructor")
    clauses
  |> compile_match table constructors return_type
