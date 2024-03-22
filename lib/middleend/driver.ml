open Common

(*
todo - potentially quite slow to test for structural equality.

can bodge fix with a mutable 'made_changes' flag passed into folding/propagation,
and set accordingly.
*)
let iter_constant_opts program =
  let max_iters = 100 in
  let rec loop_until_no_changes i p =
    if i >= max_iters then p
    else
      let p' =
        Constant_fold.constant_fold_program p
        |> Constant_propagate.const_prop_program
      in
      if p = p' then p else loop_until_no_changes (i + 1) p'
  in
  loop_until_no_changes 0 program

let run_middleend program =
  ( (if !Config.do_constant_folding_and_prop then iter_constant_opts program
     else program)
  |> Direct_calls.transform_direct_call_program
  |> fun program ->
    if !Config.do_tail_mod_monoid then
      Tail_mod_monoid.transform_tmm_program program
    else program )
  |> fun program ->
  (*if !Config.do_tail_call_elimination then Tail_call_optimise.transform_tail_call_program program else program*)
  Tail_call_optimise.transform_tail_call_program program
