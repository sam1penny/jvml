let parse_type_desugar_propagate_print s =
  Parsing.Driver.parse_string s
  |> Typing.Infer.type_program_exn_from_string "test_env"
  |> Desugar.desugar_program |> Middle_end.Constant_propagate.const_prop_program
  |> List.iter Desugar.Desugared_ast.pp_decl

let%expect_test "test simple constant propagation" =
  let program = {|
  val test = let x = 1 in 2 + x
  |} in
  let _ = parse_type_desugar_propagate_print program in
  [%expect
    {|
    └──Val test_$0
       └──Bop + : int
          └──Int 2
          └──Int 1 |}]

let%expect_test "test constant propagation simplifies if branches" =
  let program =
    {|
  val test =
  do {
    if true then 1 else 2;
    if false then 1 else 2
  }
|}
  in
  let _ = parse_type_desugar_propagate_print program in
  [%expect
    {|
    └──Val test_$0
       └──Seq
          └──Int 1
          └──Int 2 |}]

let%expect_test "test constant propagation simplifies switches" =
  let program =
    {|

  val test =
  do {
    (match [] with
      | [] -> true
      | _::_ -> false
    );
    (match [] with
      | _::_ -> true
      | _ -> false
    )
  }
  |}
  in
  let _ = parse_type_desugar_propagate_print program in
  [%expect
    {|
    └──Val test_$0
       └──Seq
          └──Shared
             └──Bool true
          └──Shared
             └──Bool false |}]
