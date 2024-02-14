let () =
  let string_program =
    {|
    type either = X of int | Y
    val to_int = fun x -> match x with X x -> x | Y -> 20
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
