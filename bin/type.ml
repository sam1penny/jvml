let () =
  let string_program = "val x = match (1, 3, 2) with (x, y, x) -> x * y * x" in
  Parsing.Driver.parse_string string_program
  |> Typing.Infer.type_program_exn_from_string string_program
  |> List.hd |> Typing.Infer.get_decl_type |> Typing.Typed_ast.pp_texpr
  |> print_endline
