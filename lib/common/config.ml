let do_peephole = ref false
let do_constant_folding_and_prop = ref false
let do_tail_call_elimination = ref false
let do_tail_mod_monoid = ref false

let set_all_opt () =
  do_constant_folding_and_prop := true;
  do_peephole := true;
  do_tail_call_elimination := true;
  do_tail_mod_monoid := true
