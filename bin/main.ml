let () =
  let string_program =
    {|
    val foo = fun x -> let y = x in (fun x -> x) val z = print(foo 3)
  |}
  in
  Parsing.Driver.parse_string string_program
  |> Typing.Infer.type_program_exn_from_string "test_env"
  |> Desugar.desugar_program |> Middle_end.Driver.run_middleend
  |> fun p ->
  List.iter Desugar.Desugared_ast.pp_decl p;
  p |> Linear.Lower.compile_program_from_scratch
  |> Linear.Instruction.show_program |> print_endline

(*
let string_program =
    {|
    val run_quad =
      let double = fun x -> x * 2 in
      let quad = fun y -> double y * double y in
      quad 4
    val test = print(run_quad)
    |}
  in
  Parsing.Driver.parse_string string_program
  |> Typing.Infer.type_program_exn_from_string "test_env"
  (*|> Desugar.desugar_program
    |> List.map Desugar.Desugared_ast.pp_decl
    |> List.hd
  *)
  |> Desugar.desugar_program
  |> List.iter Desugar.Desugared_ast.pp_decl
*)
