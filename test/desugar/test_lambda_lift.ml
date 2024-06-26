let parse_type_desugar_print s =
  Parsing.Driver.parse_string s
  |> Typing.Infer.type_program_exn_from_string "test_env"
  |> Desugar.desugared_ast_of_program |> Desugar.Unique_names.rename_program
  |> fun (renamings, program) ->
  Desugar.Lambda_lift.lift_lambdas_program renamings program
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
  [%expect
    {|
    └──Val foo_$0
       └──Fun x_$1 : int -> int -> int -> int
          └──Fun y_$1 : int -> int -> int
             └──Fun z_$0 : int -> int
                └──Bop + : int
                   └──Bop + : int
                      └──Ident x_$1 : int
                      └──Ident y_$1 : int
                   └──Ident z_$0 : int
    └──Val test_$0
       └──Let x_$0
          └──Int 1
          └──Let y_$0
             └──Int 2
             └──App
                └──App
                   └──Ident foo_$0 : int -> int -> int -> int
                   └──Ident x_$0 : int
                └──Ident y_$0 : int |}]

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
       └──Fun x_$1 : int -> int
          └──Bop * : int
             └──App
                └──Ident double_$0 : int -> int
                └──Ident x_$1 : int
             └──App
                └──Ident double_$0 : int -> int
                └──Ident x_$1 : int
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
  [%expect
    {|
    └──ValRec fact_$0
       └──Fun z_$1 : int -> int -> int
          └──Fun x_$0 : int -> int
             └──If
                └──Bop = : bool
                   └──Ident x_$0 : int
                   └──Int 0
                └──Ident z_$1 : int
                └──App
                   └──App
                      └──Ident fact_$0 : int -> int -> int
                      └──Ident z_$1 : int
                   └──Bop - : int
                      └──Ident x_$0 : int
                      └──Int 1
    └──Val test_$0
       └──Let z_$0
          └──Int 3
          └──App
             └──App
                └──Ident fact_$0 : int -> int -> int
                └──Ident z_$0 : int
             └──Int 5 |}]

let%expect_test "test lambda lift mutual recursion" =
  let program =
    {|
    val rec iseven = fun x ->
      let rec isodd = fun y -> if y = 0 then false else iseven (y - 1) in
      if x = 0 then true else isodd (x - 1)
    |}
  in
  let _ = parse_type_desugar_print program in
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
                └──App
                   └──Ident iseven_$0 : int -> bool
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
                └──App
                   └──Ident isodd_$0 : int -> bool
                   └──Bop - : int
                      └──Ident x_$0 : int
                      └──Int 1
  |}]

let%expect_test "test lambda lifting more complex mutual recursion case" =
  let program =
    {|
   val rec loop1 i =
      let rec loop2 j =
         let rec loop3 k =
            if k = 100 then ()
            else
            do {
            loop2(k);
            loop3(k+1)
         }
         in
         if j = 100 then () else loop3 (j+1)
      in
      loop2 i
   |}
  in
  let _ = parse_type_desugar_print program in
  [%expect
    {|
     └──And
        └──ValRec loop2_$0
           └──Fun j_$0 : int -> unit
              └──If
                 └──Bop = : bool
                    └──Ident j_$0 : int
                    └──Int 100
                 └──()
                 └──App
                    └──Ident loop3_$0 : int -> unit
                    └──Bop + : int
                       └──Ident j_$0 : int
                       └──Int 1
        └──ValRec loop3_$0
           └──Fun k_$0 : int -> unit
              └──If
                 └──Bop = : bool
                    └──Ident k_$0 : int
                    └──Int 100
                 └──()
                 └──Seq
                    └──App
                       └──Ident loop2_$0 : int -> unit
                       └──Ident k_$0 : int
                    └──App
                       └──Ident loop3_$0 : int -> unit
                       └──Bop + : int
                          └──Ident k_$0 : int
                          └──Int 1
        └──ValRec loop1_$0
           └──Fun i_$0 : int -> unit
              └──App
                 └──Ident loop2_$0 : int -> unit
                 └──Ident i_$0 : int |}]

let%expect_test "lambda doesn't lift printing" =
  let program = {|
  val foo =
  let f x = print x in
  f 3
  |} in
  let _ = parse_type_desugar_print program in
  [%expect
    {|
    └──Val f_$0
       └──Fun x_$0 : 'a -> unit
          └──App
             └──Ident print_$0 : 'a -> unit
             └──Ident x_$0 : 'a
    └──Val foo_$0
       └──App
          └──Ident f_$0 : int -> unit
          └──Int 3
  |}]

let%expect_test "test lambda lifting shared exprs" =
  let program =
    {|
  val foo x =
   let bar y =
      match y with
         | 0 ->
            (match x with 0 -> 0 | x -> x + y)
         | _ -> x
   in
   bar x
  |}
  in
  let _ = parse_type_desugar_print program in
  [%expect
    {|
     └──Val bar_$0
        └──Fun x_$2 : int -> int -> int
           └──Fun y_$0 : int -> int
              └──Let desugar_t1_$0
                 └──Ident y_$0 : int
                 └──Switch
                    └──Ident desugar_t1_$0 : int
                    └── <case>
                       └──Int(0)
                       └──Shared
                          └──Let desugar_t0_$0
                             └──Ident x_$2 : int
                             └──Switch
                                └──Ident desugar_t0_$0 : int
                                └── <case>
                                   └──Int(0)
                                   └──Shared
                                      └──Int 0
                                └── <fallback>
                                   └──Shared
                                      └──Let x_$1
                                         └──Ident desugar_t0_$0 : int
                                         └──Bop + : int
                                            └──Ident x_$1 : int
                                            └──Ident y_$0 : int
                    └── <fallback>
                       └──Shared
                          └──Ident x_$2 : int
     └──Val foo_$0
        └──Fun x_$0 : int -> int
           └──App
              └──App
                 └──Ident bar_$0 : int -> int -> int
                 └──Ident x_$0 : int
              └──Ident x_$0 : int

   |}]
