let () =
  let string_program =
    {|
    val rec iter = fun x -> fun y -> if x = 0 then y else iter (x - 1) y
    |}
  in
  Parsing.Driver.parse_string string_program
  |> Typing.Infer.type_program_exn_from_string string_program
  (*|> Desugar.desugar_program
    |> List.map Desugar.Desugared_ast.pp_decl
    |> List.hd
  *)
  |> Desugar.desugar_program
  |> Linear.Driver.lower_program_to_linear_ir |> Linear.Instruction.show_program
  |> print_endline
