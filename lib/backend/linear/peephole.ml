open Instruction

let rec boxunbox_opt = function
  | BOX_INT :: UNBOX_INT :: is -> boxunbox_opt is
  | BOX_BOOL :: UNBOX_BOOL :: is -> boxunbox_opt is
  | i1 :: i2 :: is -> i1 :: boxunbox_opt (i2 :: is)
  | is -> is
