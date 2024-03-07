let run_middleend program =
  Constant_fold.constant_fold_program program
  |> Constant_propagate.const_prop_program
  |> Direct_calls.transform_direct_call_program
  |> Tail_mod_monoid.transform_tmm_program
  |> Tail_call_optimise.transform_tail_call_program
