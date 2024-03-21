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

  val rec fold_left = fun f -> fun acc -> fun l ->
  match l with
      | [] -> acc
      | x::xs -> fold_left f (f acc x) xs

  val matrix_multiply = fun m1 -> fun m2 ->
  map
      (fun row ->
      mapn
          (fun column ->
          fold_left (fun x -> fun y -> x + y) 0
              (map2 (fun x -> fun y -> x * y) row column))
          m2)
      m1

  val make_matrix = fun size ->
      let rec mk_row = fun cur -> fun n ->
          if n = 0 then []
          else cur :: mk_row (cur + 1) (n - 1)
      in
      let rec mk_mat = fun size -> fun rows -> fun start ->
          if rows = 0 then []
          else (mk_row start size) :: (mk_mat (rows - 1) (start + size))
      in
      mk_mat size size 0

  val test =
  let a = make_matrix 2 in
  let b = make_matrix 2 in
  let c = matrix_multiply a b in
  do {
      print(a);
      print(b);
      print(c)
  }
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
