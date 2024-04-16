open Instruction

let rec boxunbox_opt = function
  | BOX_INT :: UNBOX_INT :: is -> boxunbox_opt is
  | BOX_FLOAT :: UNBOX_FLOAT :: is -> boxunbox_opt is
  | BOX_BOOL :: UNBOX_BOOL :: is -> boxunbox_opt is
  | i1 :: i2 :: is -> i1 :: boxunbox_opt (i2 :: is)
  | is -> is

(**
  replace:
  STORE x
  LOAD x

with:
  DUP
  STORE x

  to avoid a memory operation,
  *)
let rec storeload_opt is =
  match is with
  | STORE_REF n1 :: LOAD_REF n2 :: is when n1 = n2 ->
      DUP :: STORE_REF n1 :: storeload_opt is
  | STORE_FIELD (f1, ty) :: LOAD_FIELD (f2, _) :: is when f1 = f2 ->
      DUP :: STORE_FIELD (f1, ty) :: storeload_opt is
  | STORE_STATIC (clazz1, f1, ty) :: LOAD_STATIC (clazz2, f2, _) :: is
    when clazz1 = clazz2 && f1 = f2 ->
      DUP :: STORE_STATIC (clazz1, f1, ty) :: storeload_opt is
  | i1 :: i2 :: is -> i1 :: storeload_opt (i2 :: is)
  | is -> is

let rec gotolabel_opt is =
  match is with
  | GOTO l1 :: LABEL l2 :: is when l1 = l2 -> LABEL l2 :: gotolabel_opt is
  | i1 :: i2 :: is -> i1 :: gotolabel_opt (i2 :: is)
  | is -> is

(*
Currently occur as a result of Set_Tuple.
Also can occur in compiling, e.g
do { (); true }, or
do { 3; true }
etc
*)
let rec pushpop_opt is =
  match is with
  | PUSH_UNIT :: POP :: is -> pushpop_opt is
  | PUSH_BOOL _ :: POP :: is -> pushpop_opt is
  | PUSH_INT _ :: POP :: is -> pushpop_opt is
  | i1 :: i2 :: is -> i1 :: pushpop_opt (i2 :: is)
  | is -> is
