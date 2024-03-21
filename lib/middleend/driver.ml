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
  iter_constant_opts program |> Direct_calls.transform_direct_call_program
  |> Tail_mod_monoid.transform_tmm_program
  |> Tail_call_optimise.transform_tail_call_program
