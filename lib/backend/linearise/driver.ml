let lower_decl_to_linear_ir ast =
  Lower.compile_decl_from_scratch ast |> Optimise.run_optimisations

let lower_program_to_linear_ir ast =
  Lower.compile_program_from_scratch ast |> Optimise.run_optimisations
