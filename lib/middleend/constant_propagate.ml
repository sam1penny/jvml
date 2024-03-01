open Desugar.Desugared_ast

let rec const_prop_expr const_tbl e =
  (*Desugared_ast.pp_expr e;*)
  let rec_const_prop = const_prop_expr const_tbl in
  match e with
  | Ident (_, x) -> (
      match Hashtbl.find_opt const_tbl x with None -> e | Some c -> c)
  | Let (ty, x, e0, e1) -> (
      match e0 with
      | Int _ | Bool _ | Unit ->
          let _ = Hashtbl.add const_tbl x e0 in
          Let (ty, x, rec_const_prop e0, rec_const_prop e1)
      | _ ->
          Let (ty, x, rec_const_prop e0, rec_const_prop e1)
          (* todo - consider transforming into A-normal form to simplify constant propagation.

             Only cost is, always assigning to variables (let x = 1 in let y = 2 in x + y) has significant performance decrease.

             But maybe these disappear after optimisations?
          *))
  | _ -> Desugar.Utils.map_over_sub_expr rec_const_prop e

let const_prop_decl const_tbl decl =
  Desugar.Utils.map_over_decl_exprs (const_prop_expr const_tbl) decl

let const_prop_program program =
  let const_tbl = Hashtbl.create 10 in
  List.map (const_prop_decl const_tbl) program
