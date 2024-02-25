let () =
  let string_program =
    {|
    val add = fun x -> fun y -> x + y
    val z = add 3 4
    val p = let add3 = add 3 in add3 4
    val rec fact = fun x -> if x = 0 then 1 else x * fact (x - 1)
    |}
  in
  Parsing.Driver.parse_string string_program
  |> Typing.Infer.type_program_exn_from_string string_program
  (*|> Desugar.desugar_program
    |> List.map Desugar.Desugared_ast.pp_decl
    |> List.hd
  *)
  |> Desugar.desugar_program
  |> List.iter Desugar.Desugared_ast.pp_decl
