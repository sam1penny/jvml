let () =
  let string_program =
    {|

    val rec map = fun f -> fun l ->
      match l with
          | [] -> []
          | x::xs -> f x :: (map f xs)


  val head = fun l ->
      match l with
          | [] -> 0
          | x::_ -> x

  val tail = fun l ->
      match l with
          | [] -> []
          | _::xs -> xs

  val rec map2 = fun f -> fun a -> fun b ->
      match (a, b) with
          | ([], []) -> []
          | ((a::as), (b::bs)) -> f a b :: (map2 f as bs)
          | _ -> []

  val rec mapn = fun f -> fun lists ->
    match lists with
      | []::others -> []
      | _ -> (f (map head lists)) :: (mapn f (map tail lists))
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
