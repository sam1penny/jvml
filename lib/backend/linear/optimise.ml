open Instruction

let do_peephole = true

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

let run_optimisations program =
  if do_peephole then
    apply_over_instructions Peephole.boxunbox_opt program
    |> apply_over_instructions Peephole.storeload_opt
    |> apply_over_instructions Peephole.gotolabel_opt
  else program
