(* todo - move this into backend *)

open Desugared_ast

let rec has_tail_call_expr fn_name e =
  let rec_has_tail_call = has_tail_call_expr fn_name in
  match e with
  | Int _ | Ident _ | Bool _ | Unit | Constr _ | Match_Failure -> false
  | Bop _ -> false
  | If (_, _, e1, e2) -> rec_has_tail_call e1 || rec_has_tail_call e2
  | Fun _ -> false
  | App _ -> false
  | Direct_app (_, _, name, _) -> name = fn_name
  | Tuple _ -> false
  | Let (_, _, _, e1) -> rec_has_tail_call e1
  | LetRec (_, _, _, e1) -> rec_has_tail_call e1
  | Seq (_, es) -> List.rev es |> List.hd |> rec_has_tail_call
  | TupleGet _ -> false
  | ConstructorGet _ -> false
  | Switch (_, _, cases, maybe_fallback_expr) ->
      List.exists (fun (_, case_expr) -> rec_has_tail_call case_expr) cases
      |> ( || )
           (Option.map
              (fun fallback_expr -> rec_has_tail_call fallback_expr)
              maybe_fallback_expr
           |> Option.value ~default:false)
  | Shared_Expr (e_ref, _) -> rec_has_tail_call !e_ref

let has_tail_call_decl decl =
  match decl with
  | ValRec (_, x, e) ->
      let _, body = Lambda_lift.collect_funargs e in
      has_tail_call_expr x body
  | _ -> false
