let () =
  let string_program =
    {|
    type 'a list = N | C of 'a * 'a list
    val rec doublelist = fun l ->
      match l with
        | N -> N
        | C (hd, N) -> C (2 * hd, N)
        | C (hd, tl) -> C (3 * hd, tl)
    |}
  in
  Parsing.Driver.parse_string string_program
  |> Typing.Infer.type_program_exn_from_string string_program
  |> Desugar.desugar_program
  |> List.iter (fun decl -> Desugar.Desugared_ast.pp_decl decl)
