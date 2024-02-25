let () =
  let string_program = {|
    val x = print(3)
    val y = x
    |} in
  Parsing.Driver.parse_string string_program
  |> Typing.Infer.type_program_exn_from_string string_program
  (*|> Desugar.desugar_program
    |> List.map Desugar.Desugared_ast.pp_decl
    |> List.hd
  *)
  |> Desugar.desugar_program
  |> Linear.Driver.lower_program_to_linear_ir |> Jvm.Driver.lower_ir
  |> print_endline
