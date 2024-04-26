let parse_type_desugar_inline_print number_inline_iterations s =
  let rec loop f acc n = if n = 0 then acc else loop f (f acc) (n - 1) in
  Parsing.Driver.parse_string s
  |> Typing.Infer.type_program_exn_from_string "test_env"
  |> Desugar.desugar_program
  |> fun program ->
  loop Middle_end.Inline.inline_program program number_inline_iterations
  |> List.iter Desugar.Desugared_ast.pp_decl

let%expect_test "test avoiding inlining of side-effecting expression" =
  let program = {|
  val foo =
    let x = print(3) in
    x
  |} in
  let _ = parse_type_desugar_inline_print 1 program in
  [%expect
    {|
    └──Val foo_$0
       └──Let x_$0
          └──App
             └──Ident print_$0 : int -> unit
             └──Int 3
          └──Ident x_$0 : unit |}]

let%expect_test "small MultiUnsafe function is inlined" =
  let program = {|
  val double x = x * 2
  val foo =
    double 3
  |} in
  let _ = parse_type_desugar_inline_print 2 program in
  [%expect
    {|
    └──Val double_$0
       └──Fun x_$0 : int -> int
          └──Bop * : int
             └──Ident x_$0 : int
             └──Int 2
    └──Val foo_$0
       └──Let x_$0_$0
          └──Int 3
          └──Bop * : int
             └──Int 3
             └──Int 2 |}]

let%expect_test "large MultiUnsafe function is not inlined, smaller \
                 MultiUnsafe function is" =
  let program =
    {|
  val small_fun = fun x -> x + x + x + x + x
  val large_fun = fun x -> x + x + x + x + x + x
  val bar =
    do {
      small_fun 3;
      large_fun 4
    }
  |}
  in
  let _ = parse_type_desugar_inline_print 3 program in
  [%expect
    {|
    └──Val small_fun_$0
       └──Fun x_$0 : int -> int
          └──Bop + : int
             └──Bop + : int
                └──Bop + : int
                   └──Bop + : int
                      └──Ident x_$0 : int
                      └──Ident x_$0 : int
                   └──Ident x_$0 : int
                └──Ident x_$0 : int
             └──Ident x_$0 : int
    └──Val large_fun_$0
       └──Fun x_$1 : int -> int
          └──Bop + : int
             └──Bop + : int
                └──Bop + : int
                   └──Bop + : int
                      └──Bop + : int
                         └──Ident x_$1 : int
                         └──Ident x_$1 : int
                      └──Ident x_$1 : int
                   └──Ident x_$1 : int
                └──Ident x_$1 : int
             └──Ident x_$1 : int
    └──Val bar_$0
       └──Seq
          └──Bop + : int
             └──Bop + : int
                └──Bop + : int
                   └──Bop + : int
                      └──Int 3
                      └──Int 3
                   └──Int 3
                └──Int 3
             └──Int 3
          └──App
             └──Ident large_fun_$0 : int -> int
             └──Int 4

  |}]
