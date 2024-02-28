let parse_type_desugar_print s =
  Parsing.Driver.parse_string s
  |> Typing.Infer.type_program_exn_from_string "test_env"
  |> Desugar.desugar_program
  |> List.iter Desugar.Desugared_ast.pp_decl

let%expect_test "test add captured lifted arguments" =
  let program =
    {|
    val test =
      let x = 1 in
      let y = 2 in
      let foo = fun z -> x + y + z in
      foo
  |}
  in
  let _ = parse_type_desugar_print program in
  [%expect {||}]

let%expect_test "test avoid capturing of already lifted argument" =
  let program =
    {|
    val test =
      let double = fun x -> x * 2 in
      let quad = fun x -> double x * double x in
      print (quad 4)
  |}
  in
  let _ = parse_type_desugar_print program in
  [%expect
    {|
    └──Val double_$0
       └──Fun x_$0 : int -> int
          └──Bop * : int
             └──Ident x_$0 : int
             └──Int 2
    └──Val quad_$0
       └──Fun x_$0 : int -> int
          └──Bop * : int
             └──Direct_app : double_$0
                └──Ident x_$0 : int
             └──Direct_app : double_$0
                └──Ident x_$0 : int
    └──Val test_$0
       └──App
          └──Ident print_$0 : int -> unit
          └──App
             └──Ident quad_$0 : int -> int
             └──Int 4 |}]

let%expect_test "test capturing recursive inner function" =
  let program =
    {|
  val test =
    let z = 3 in
    let rec fact = fun x -> if x = 0 then z else fact (x - 1)
    in
    fact 5
  |}
  in
  let _ = parse_type_desugar_print program in
  [%expect {||}]