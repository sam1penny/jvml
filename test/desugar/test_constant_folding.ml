let parse_type_desugar_fold_print s =
  Parsing.Driver.parse_string s
  |> Typing.Infer.type_program_exn_from_string "test_env"
  |> Desugar.desugar_program |> Middle_end.Constant_fold.constant_fold_program
  |> List.iter Desugar.Desugared_ast.pp_decl

let%expect_test "test simple constant folding" =
  let program =
    {|
  val test_add = 1 + 2
  val test_mul = 1 + 3 * 4
  val test_div = 5 / 2
  val test_sub = 5 - 3
  val test_gt = 1 > 2
  val test_lt = 1 < 2
  val test_and = true && true
  val test_or = false || true
  |}
  in
  let _ = parse_type_desugar_fold_print program in
  [%expect
    {|
    └──Val test_add_$0
       └──Int 3
    └──Val test_mul_$0
       └──Int 13
    └──Val test_div_$0
       └──Int 2
    └──Val test_sub_$0
       └──Int 2
    └──Val test_gt_$0
       └──Bool false
    └──Val test_lt_$0
       └──Bool true
    └──Val test_and_$0
       └──Bool true
    └──Val test_or_$0
       └──Bool true |}]

let%expect_test "test constant folding does not fold side effect" =
  let program = {|
  val test1 = 1 / 0
  val test2 = 1 / (5 - 5)
  |} in
  let _ = parse_type_desugar_fold_print program in
  [%expect
    {|
    └──Val test1_$0
       └──Bop / : int
          └──Int 1
          └──Int 0
    └──Val test2_$0
       └──Bop / : int
          └──Int 1
          └──Int 0

  |}]

let%expect_test "test constant folding with long dependencies" =
  let program =
    {|
  val test1 = fun x -> 1 + x + 1
  val test2 = fun x -> 1 + x + x + x + 4 + x + 3
  |}
  in
  let _ = parse_type_desugar_fold_print program in
  [%expect
    {|
    └──Val test1_$0
       └──Fun x_$0 : int -> int
          └──Bop + : int
             └──Ident x_$0 : int
             └──Int 2
    └──Val test2_$0
       └──Fun x_$0 : int -> int
          └──Bop + : int
             └──Bop + : int
                └──Bop + : int
                   └──Bop + : int
                      └──Ident x_$0 : int
                      └──Ident x_$0 : int
                   └──Ident x_$0 : int
                └──Ident x_$0 : int
             └──Int 8

  |}]
