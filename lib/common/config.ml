let do_peephole = ref false
let do_constant_folding_and_prop = ref false
let do_tail_call_elimination = ref false
let do_tail_mod_monoid = ref false
let do_tail_mod_cons = ref false
let do_inlining = ref false
let inlining_score_threshold = ref 10
let use_dynamic_lambdas = ref false

(*
Default name for a class is 'Foo' if unset.

Otherwise set by command line option
*)
let generated_class_name = ref "Foo"

let set_all_opt () =
  do_constant_folding_and_prop := true;
  do_peephole := true;
  do_tail_call_elimination := true;
  do_tail_mod_monoid := true;
  do_tail_mod_cons := true;
  do_inlining := true
