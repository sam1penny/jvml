let () =
  let string_program = {|
    val foo = []
    val bar = 3 :: []
    |} in
  Parsing.Driver.parse_string string_program
  |> Typing.Infer.type_program_exn_from_string string_program
  |> List.iter (fun d ->
         Typing.Infer.get_decl_type d
         |> Typing.Typed_ast.pp_texpr |> print_endline)
