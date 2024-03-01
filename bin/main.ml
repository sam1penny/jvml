let () =
  let string_program =
    {|
  val foo = fun x -> 1 + x + 3
  val test = 1 + 4 * 5
  val test2 = fun x -> 1 + x + x + 3
  val test3 = fun x -> 1 + x * x + 1 * 3
  |}
  in
  Parsing.Driver.parse_string string_program
  |> Typing.Infer.type_program_exn_from_string "test_env"
  |> Desugar.desugar_program
  |> fun p ->
  List.iter Desugar.Desugared_ast.pp_decl p;
  p
  |> List.map Desugar.Tail_call_optimise.has_tail_call_decl
  |> List.iter (fun b -> print_endline @@ string_of_bool b)

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
