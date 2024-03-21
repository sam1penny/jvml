let () =
  let string_program =
    {|
  val rec map = fun f -> fun l ->
    match l with
        | [] -> []
        | x::xs -> f x :: (map f xs)

  val foo =
    let x = [1; 2; 3] in
    let y = [true; false] in
    let a = map (fun x -> x + 1) x in
    let b = map (fun x -> x && true) y in
    do {print(a); print(b)}
  |}
  in
  Parsing.Driver.parse_string string_program
  |> Typing.Infer.type_program_exn_from_string "test_env"
  |> Desugar.desugar_program |> Middle_end.Driver.run_middleend
  |> fun p -> List.iter Desugar.Desugared_ast.pp_decl p

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
