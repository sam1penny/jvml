let parse_type_desugar_direct_print s =
  Parsing.Driver.parse_string s
  |> Typing.Infer.type_program_exn_from_string "test_env"
  |> Desugar.desugar_program
  |> List.iter Desugar.Desugared_ast.pp_decl

let%expect_test "test single arg direct call toplevel definition" =
  let program =
    {|
  val foo = fun x -> x + 1
  val rec bar = fun x -> bar (x - 1)
  val test = do {
    foo 3;
    bar 4
  }
  |}
  in
  let _ = parse_type_desugar_direct_print program in
  [%expect
    {|
    └──Val foo_$0
       └──Fun x_$0 : int -> int
          └──Bop + : int
             └──Ident x_$0 : int
             └──Int 1
    └──ValRec bar_$0
       └──Fun x_$1 : int -> 'a
          └──Direct_app : bar_$0
             └──Bop - : int
                └──Ident x_$1 : int
                └──Int 1
    └──Val test_$0
       └──Seq
          └──Direct_app : foo_$0
             └──Int 3
          └──Direct_app : bar_$0
             └──Int 4
  |}]

let%expect_test "test single arg direct call nested let" =
  let program =
    {|
  val foo = let bar = fun x -> x + 1 in bar 3
  val rec baz = fun z -> let rec qux = fun x -> qux (x - 1) in qux 5
  |}
  in
  let _ = parse_type_desugar_direct_print program in
  [%expect
    {|
    └──Val bar_$0
       └──Fun x_$0 : int -> int
          └──Bop + : int
             └──Ident x_$0 : int
             └──Int 1
    └──Val foo_$0
       └──Direct_app : bar_$0
          └──Int 3
    └──ValRec qux_$0
       └──Fun x_$1 : int -> 'c
          └──Direct_app : qux_$0
             └──Bop - : int
                └──Ident x_$1 : int
                └──Int 1
    └──ValRec baz_$0
       └──Fun z_$0 : 'a -> 'b
          └──Direct_app : qux_$0
             └──Int 5
  |}]

let%expect_test "test direct call on mutual recursion" =
  let program =
    {|
      val rec iseven = fun x ->
      let rec isodd = fun y -> if y = 0 then false else iseven (y - 1) in
      if x = 0 then true else isodd (x - 1)
      |}
  in
  let _ = parse_type_desugar_direct_print program in
  [%expect
    {|
     └──And
        └──ValRec isodd_$0
           └──Fun y_$0 : int -> bool
              └──If
                 └──Bop = : bool
                    └──Ident y_$0 : int
                    └──Int 0
                 └──Bool false
                 └──Direct_app : iseven_$0
                    └──Bop - : int
                       └──Ident y_$0 : int
                       └──Int 1
        └──ValRec iseven_$0
           └──Fun x_$0 : int -> bool
              └──If
                 └──Bop = : bool
                    └──Ident x_$0 : int
                    └──Int 0
                 └──Bool true
                 └──Direct_app : isodd_$0
                    └──Bop - : int
                       └──Ident x_$0 : int
                       └──Int 1 |}]
