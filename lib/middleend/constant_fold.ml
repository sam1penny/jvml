open Desugar.Desugared_ast
open Common

let apply_int_bop bop i1 i2 =
  match bop with
  | ADD -> Int32.add i1 i2
  | SUB -> Int32.sub i1 i2
  | MUL -> Int32.mul i1 i2
  | DIV -> Int32.div i1 i2
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
let constant_fold_bop ty e0 bop e1 =
  match (bop, e0, e1) with
  (* any integer bop with no side effects
     i0 + i1
  *)
  | (ADD | SUB | MUL), Int i0, Int i1 -> Int (apply_int_bop bop i0 i1)
  (* associative + commutative => foldable
     1. (x + i0) + i1 -> x + (i0 + i1)
     2. (i0 + x) + i1 -> x + (i0 + i1)
     3. i0 + (i1 + x) -> x + (i0 + i1)
     4. i0 + (x + i1) -> x + (i0 + i1)
  *)
  | ADD, Bop (_, e0_0, ADD, Int i0_1), Int i1 ->
      Bop (ty, e0_0, ADD, Int (Int32.add i0_1 i1))
  | ADD, Bop (_, Int i0_0, ADD, e0_1), Int i1 ->
      Bop (ty, e0_1, ADD, Int (Int32.add i0_0 i1))
  | ADD, Int i0, Bop (_, Int i1_0, ADD, e1_1) ->
      Bop (ty, Int (Int32.add i0 i1_0), ADD, e1_1)
  | ADD, Int i0, Bop (_, e1_0, ADD, Int i1_1) ->
      Bop (ty, Int (Int32.add i0 i1_1), ADD, e1_0)
  | MUL, Bop (_, e0_0, MUL, Int i0_1), Int i1 ->
      Bop (ty, e0_0, MUL, Int (Int32.mul i0_1 i1))
  | MUL, Bop (_, Int i0_0, MUL, e0_1), Int i1 ->
      Bop (ty, e0_1, MUL, Int (Int32.mul i0_0 i1))
  | MUL, Int i0, Bop (_, Int i1_0, MUL, e1_1) ->
      Bop (ty, Int (Int32.mul i0 i1_0), MUL, e1_1)
  | MUL, Int i0, Bop (_, e1_0, MUL, Int i1_1) ->
      Bop (ty, Int (Int32.mul i0 i1_1), MUL, e1_0)
  (* associative + commutative + exists constant => pull it out for parent call
     (x + i) + x -> (x + x) + i
     (i + x) + x -> (x + x) + i
     x + (x + i) -> (x + x) + i
     x + (i + x) -> (x + x) + i
  *)
  | ADD, Bop (ty', e0_0, ADD, Int i0_1), e1 ->
      Bop (ty, Bop (ty', e0_0, ADD, e1), ADD, Int i0_1)
  | ADD, Bop (ty', Int i0_0, ADD, e0_1), e1 ->
      Bop (ty, Bop (ty', e0_1, ADD, e1), ADD, Int i0_0)
  | ADD, e0, Bop (ty', e1_0, ADD, Int i1_1) ->
      Bop (ty, Bop (ty', e0, ADD, e1_0), ADD, Int i1_1)
  | ADD, e0, Bop (ty', Int i1_0, ADD, e1_1) ->
      Bop (ty, Bop (ty', e0, ADD, e1_1), ADD, Int i1_0)
  | MUL, Bop (ty', e0_0, MUL, Int i0_1), e1 ->
      Bop (ty, Bop (ty', e0_0, MUL, e1), MUL, Int i0_1)
  | MUL, Bop (ty', Int i0_0, MUL, e0_1), e1 ->
      Bop (ty, Bop (ty', e0_1, MUL, e1), MUL, Int i0_0)
  | MUL, e0, Bop (ty', e1_0, MUL, Int i1_1) ->
      Bop (ty, Bop (ty', e0, MUL, e1_0), MUL, Int i1_1)
  | MUL, e0, Bop (ty', Int i1_0, MUL, e1_1) ->
      Bop (ty, Bop (ty', e0, MUL, e1_1), MUL, Int i1_0)
  | DIV, Int i0, Int i1 when i1 <> 0l -> Int (Int32.div i0 i1)
  | LT, Int i0, Int i1 -> Bool (i0 < i1)
  | GT, Int i0, Int i1 -> Bool (i0 > i1)
  | AND, Bool b1, Bool b2 -> Bool (b1 && b2)
  | OR, Bool b1, Bool b2 -> Bool (b1 || b2)
  | bop, e0, e1 -> Bop (ty, e0, bop, e1)

let rec constant_fold_expr e =
  match e with
  | Bop (ty, e0, bop, e1) ->
      let e0' = constant_fold_expr e0 in
      let e1' = constant_fold_expr e1 in
      constant_fold_bop ty e0' bop e1'
  | e -> Desugar.Utils.map_over_sub_expr constant_fold_expr e

let constant_fold_decl d =
  let folded_decl = Desugar.Utils.map_over_decl_exprs constant_fold_expr d in
  Desugar.Utils.clear_shared_decl_seen folded_decl;
  folded_decl

let constant_fold_program program = List.map constant_fold_decl program
