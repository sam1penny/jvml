let () =
  let string_program =
    {|
    val x = match 1 with
        | 0 -> 0
        | 1 -> 0
        | x -> x - 1
    |}
  in
  Parsing.Driver.parse_string string_program
  |> Typing.Infer.type_program_exn_from_string string_program
  |> Desugar.desugar_program |> Linear.Driver.lower_program_to_linear_ir
  |> Linear.Instruction.show_program |> print_endline
