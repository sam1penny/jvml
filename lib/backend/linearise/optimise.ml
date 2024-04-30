open Instruction
open Common

let apply_over_instructions f program =
  {
    Instruction.declarations =
      List.map
        (function Closure c -> Closure { c with body = f c.body } | d -> d)
        program.declarations;
    Instruction.code = f program.code;
    Instruction.static_methods =
      List.map
        (fun sm -> { sm with Instruction.body = f sm.body })
        program.static_methods;
  }

let maybe_run_peephole condition peephole program =
  if condition then apply_over_instructions peephole program else program

let run_optimisations program =
  if !Config.do_peephole then
    maybe_run_peephole !Config.do_peep_box Peephole.boxunbox_opt program
    |> maybe_run_peephole !Config.do_peep_store_load Peephole.storeload_opt
    |> maybe_run_peephole !Config.do_peep_goto_label Peephole.gotolabel_opt
    |> maybe_run_peephole !Config.do_peep_push_pop Peephole.pushpop_opt
  else program
