open Desugared_ast
open Typing
open Common

type clause =
  | Clause of (Desugared_ast.expr * Typed_ast.pattern) list * Desugared_ast.expr

module ConSet = Set.Make (struct
  type t = con

  let compare = compare
end)

let clause_of_case (pattern, expr) = Clause ([ (Obj, pattern) ], expr)

let get_tname = function
  | Typed_ast.TyCustom (_, tname) -> tname
  | _ -> raise @@ Failure "called get_tname on type_expr that is not TyCustom"

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

let rec compile_match (constructors : con StringMap.t StringMap.t)
    (clauses : clause list) : expr =
  let clauses = List.map move_variable_patterns clauses in
  let clauses = List.map move_wildcard_patterns clauses in
  match clauses with
  | [] -> Match_Failure
  | Clause ([], body) :: _ -> body
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
          compile_constructor_match constructors clauses branch_var
            (StringMap.find (get_tname ty) constructors)
      | Typed_ast.Pat_Int _ -> compile_int_match constructors clauses branch_var
      | Typed_ast.Pat_Tuple _ ->
          compile_tuple_match constructors clauses branch_var
      | _ -> raise @@ Failure "")

and compile_constructor_match constructors (clauses : clause list)
    (branch_var : expr) (constructors_by_name : con StringMap.t) =
  List.fold_right
    (fun (Clause (pats, body)) (case_clauses, fallback_clauses) ->
      match List.assoc_opt branch_var pats with
      | None -> (case_clauses, Clause (pats, body) :: fallback_clauses)
      | Some (Typed_ast.Pat_Constr (_, _, cname, parg_opt)) ->
          let con = StringMap.find cname constructors_by_name in
          let existing_clauses_for_case =
            List.assoc_opt con case_clauses |> Option.value ~default:[]
          in
          let maybe_extra_test =
            Option.map (fun parg -> (ConstructorGet branch_var, parg)) parg_opt
            |> Option.to_list
          in
          let new_case_clauses =
            ( con,
              Clause (List.remove_assoc branch_var pats @ maybe_extra_test, body)
              :: existing_clauses_for_case )
            :: List.remove_assoc con case_clauses
          in
          (new_case_clauses, fallback_clauses)
      | _ ->
          raise
          @@ Failure
               "illegal state - all cells in a column must be constructors")
    clauses ([], [])
  |> fun (case_clauses, fallback_clauses) ->
  let compiled_cases =
    List.map
      (fun (con, nested_match) ->
        (con, compile_match constructors (nested_match @ fallback_clauses)))
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
    else Some (compile_match constructors fallback_clauses)
  in
  Switch (branch_var, compiled_cases, fallback_clauses_opt)

and compile_int_match constructors (clauses : clause list) (branch_var : expr) =
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
        (con, compile_match constructors (nested_match @ fallback_clauses)))
      case_clauses
  in
  Switch
    ( branch_var,
      compiled_cases,
      Some (compile_match constructors fallback_clauses) )

and compile_tuple_match constructors (clauses : clause list) (branch_var : expr)
    =
  List.map
    (fun (Clause (pats, body)) ->
      match List.assoc_opt branch_var pats with
      | None -> Clause (pats, body)
      | Some (Typed_ast.Pat_Tuple (_, _, nested_pats)) ->
          let vars =
            List.mapi (fun i _ -> TupleGet (i, branch_var)) nested_pats
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
  |> compile_match constructors
