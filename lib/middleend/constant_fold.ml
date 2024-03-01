open Desugar.Desugared_ast
open Common

let apply_int_bop bop i1 i2 =
  match bop with
  | ADD -> i1 + i2
  | SUB -> i1 - i2
  | MUL -> i1 * i2
  | DIV -> i1 / i2
  | _ -> raise @@ Failure "called apply_int_bop to non-int bop!"

(*
due to associativity rules, e.g:
(x + 1) + 1 can be folded to 2 + x

due to associativity and commutativity rules, we can rewrite
(1 + x) + 1 as
(x + 1) + 1
and fold to x + 2

in general, we move constants to the right and fold where possible
*)
let rec constant_fold_bop ty e0 bop e1 =
  match (bop, e0, e1) with
  (* any integer bop with no side effects *)
  | (ADD | SUB | MUL), Int i0, Int i1 -> Int (apply_int_bop bop i0 i1)
  (* associative bop with no side effects *)
  | ADD, Bop (_, e0_0, ADD, Int i0_1), Int i1 ->
      Bop (ty, e0_0, ADD, Int (i0_1 + i1))
  | MUL, Bop (_, e0_0, ADD, Int i0_1), Int i1 ->
      Bop (ty, e0_0, MUL, Int (i0_1 + i1))
  (* associative + commutative, move constant to right for subcall *)
  | ADD, Bop (ty', e0_0, ADD, Int i0_1), e1 ->
      constant_fold_bop ty (Bop (ty', e0_0, ADD, e1)) ADD (Int i0_1)
  | MUL, Bop (ty', e0_0, MUL, Int i0_1), e1 ->
      constant_fold_bop ty (Bop (ty', e0_0, MUL, e1)) MUL (Int i0_1)
  | ADD, (Int _ as e0), e1 -> constant_fold_bop ty e1 ADD e0
  | MUL, (Int _ as e0), e1 -> constant_fold_bop ty e1 MUL e0
  | DIV, Int i0, Int i1 when i1 <> 0 -> Int (i0 / i1)
  | LT, Int i0, Int i1 -> Bool (i0 < i1)
  | GT, Int i0, Int i1 -> Bool (i0 > i1)
  | AND, Bool b1, Bool b2 -> Bool (b1 && b2)
  | OR, Bool b1, Bool b2 -> Bool (b1 || b2)
  | bop, e0, e1 -> Bop (ty, e0, bop, e1)

and constant_fold_expr e =
  match e with
  | Bop (ty, e0, bop, e1) ->
      let e0' = constant_fold_expr e0 in
      let e1' = constant_fold_expr e1 in
      constant_fold_bop ty e0' bop e1'
  | e -> Desugar.Utils.map_over_sub_expr constant_fold_expr e

let constant_fold_decl = Desugar.Utils.map_over_decl_exprs constant_fold_expr
let constant_fold_program program = List.map constant_fold_decl program
