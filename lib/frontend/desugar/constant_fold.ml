open Desugared_ast
open Common

let map_over_sub_expr f e =
  match e with
  | Int _ | Ident _ | Bool _ | Unit | Constr _ | Match_Failure -> e
  | Bop (ty, e0, bop, e1) ->
      let e0' = f e0 in
      let e1' = f e1 in
      Bop (ty, e0', bop, e1')
  | If (ty, e0, e1, e2) ->
      let e0' = f e0 in
      let e1' = f e1 in
      let e2' = f e2 in
      If (ty, e0', e1', e2')
  | Fun (t0, t1, x, e) -> Fun (t0, t1, x, f e)
  | App (ty, e0, e1) ->
      let e0' = f e0 in
      let e1' = f e1 in
      App (ty, e0', e1')
  | Direct_app (ret_ty, args_ty, name, es) ->
      Direct_app (ret_ty, args_ty, name, List.map f es)
  | Tuple (ty, es) -> Tuple (ty, List.map f es)
  | Let (ty, x, e0, e1) ->
      let e0' = f e0 in
      let e1' = f e1 in
      Let (ty, x, e0', e1')
  | LetRec (ty, x, e0, e1) ->
      let e0' = f e0 in
      let e1' = f e1 in
      LetRec (ty, x, e0', e1')
  | Seq (ty, es) -> Seq (ty, List.map f es)
  | TupleGet (ty, i, e) -> TupleGet (ty, i, f e)
  | ConstructorGet (ty, c, e) -> ConstructorGet (ty, c, f e)
  | Switch (ty, e0, cases, maybe_fallback_expr) ->
      let e0' = f e0 in
      let cases' =
        List.map (fun (con, case_expr) -> (con, f case_expr)) cases
      in
      Switch (ty, e0', cases', Option.map f maybe_fallback_expr)
  | Shared_Expr (expr_ref, label_opt) ->
      (* todo - add general bool seen flag to avoid unnecessary recomputation *)
      expr_ref := f !expr_ref;
      Shared_Expr (expr_ref, label_opt)

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
  | e -> map_over_sub_expr constant_fold_expr e

let constant_fold_decl = Deadcode.map_over_decl_exprs constant_fold_expr
let constant_fold_program program = List.map constant_fold_decl program
