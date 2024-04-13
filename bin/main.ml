let () =
  let string_program =
    {|
    val rec rebuild = fun l ->
      match l with
        | [] -> []
        | x::xs -> (2 * x) :: rebuild xs

    val test = print(rebuild [1;2;3])
  |}
  in
  Common.Config.set_all_opt ();
  Parsing.Driver.parse_string string_program
  |> Typing.Infer.type_program_exn_from_string string_program
  |> Desugar.desugar_program |> Middle_end.Driver.run_middleend
  |> List.iter Desugar.Desugared_ast.pp_decl
