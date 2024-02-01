let compile_single_decl s =
  Parsing.Driver.parse_string s
  |> Typing.Driver.type_program |> Result.get_ok |> List.hd
  |> Lower.compile_decl_from_scratch |> Optimise.run_optimisations

let compile_program s =
  Parsing.Driver.parse_string s
  |> Typing.Driver.type_program |> Result.get_ok
  |> Lower.compile_program_from_scratch |> Optimise.run_optimisations
