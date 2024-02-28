let () =
  let string_program =
    {|
  val rec x = fun f -> x f
  val rec y = fun f -> 1 + y f
  type 'a list = N | C of 'a * 'a list
  val rec length = fun xs ->
    match xs with
      | N -> 0
      | C(_, xs) -> 1 + length xs

  val rec length_tr = fun xs -> fun acc ->
    match xs with
      | N -> acc
      | C(_, xs) -> length_tr xs (acc+1)

  |}
  in
  Parsing.Driver.parse_string string_program
  |> Typing.Infer.type_program_exn_from_string "test_env"
  |> Desugar.desugar_program
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
