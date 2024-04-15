let parse_type_desugar_tco_print s =
  Parsing.Driver.parse_string s
  |> Typing.Infer.type_program_exn_from_string "test_env"
  |> Desugar.desugar_program
  |> Middle_end.Direct_calls.transform_direct_call_program
  |> Middle_end.Tail_call_optimise.transform_tail_call_program
  |> List.iter Desugar.Desugared_ast.pp_decl

let%expect_test "test one-arg tail call" =
  let program =
    {|
  val rec count = fun n -> if n = 0 then 1 else count (n - 1)
  |}
  in
  let _ = parse_type_desugar_tco_print program in
  [%expect
    {|
    └──Val count_$0
       └──Fun n_$0 : int -> int
          └──While true
             └──If
                └──Bop = : bool
                   └──Ident n_$0 : int
                   └──Int 0
                └──Break
                   └──Int 1
                └──Assign_Seq
                   └── assign n_$0 : int =
                      └──Bop - : int
                         └──Ident n_$0 : int
                         └──Int 1

  |}]

let%expect_test "test multiple arg tail call" =
  let program =
    {|
  val rec count2 = fun x -> fun y ->
    if x = 0 then y
    else count2 (x - 1) y
  |}
  in
  let _ = parse_type_desugar_tco_print program in
  [%expect
    {|
    └──Val count2_$0
       └──Fun x_$0 : int -> 'a -> 'a
          └──Fun y_$0 : 'a -> 'a
             └──While true
                └──If
                   └──Bop = : bool
                      └──Ident x_$0 : int
                      └──Int 0
                   └──Break
                      └──Ident y_$0 : 'a
                   └──Assign_Seq
                      └── assign x_$0 : int =
                         └──Bop - : int
                            └──Ident x_$0 : int
                            └──Int 1
                      └── assign y_$0 : 'a =
                         └──Ident y_$0 : 'a

  |}]

let%expect_test "test tail call with match" =
  let program =
    {|
  val rec count = fun x ->
    match x with
      | 0 -> 1
      | _ -> count (x - 1)
  |}
  in
  let _ = parse_type_desugar_tco_print program in
  [%expect
    {|
    └──Val count_$0
       └──Fun x_$0 : int -> int
          └──While true
             └──Let desugar_t0_$0
                └──Ident x_$0 : int
                └──Switch
                   └──Ident desugar_t0_$0 : int
                   └── <case>
                      └──Int(0)
                      └──Break
                         └──Shared
                            └──Int 1
                   └── <fallback>
                      └──Shared
                         └──Assign_Seq
                            └── assign x_$0 : int =
                               └──Bop - : int
                                  └──Ident x_$0 : int
                                  └──Int 1

  |}]

let%expect_test "test tail call with ADT" =
  let program =
    {|
  type 'a my_list = N | C of 'a * 'a my_list
  val rec length_tr = fun l -> fun acc ->
    match l with
      | N -> acc
      | C(_, tl) -> length_tr tl (acc+1)
  |}
  in
  let _ = parse_type_desugar_tco_print program in
  [%expect
    {|
    └──Type my_list
       └──params = ['a]
       └──constructors
          └──N : tag=0
          └──C of ('a * 'a my_list) : tag=1
    └──Val length_tr_$0
       └──Fun l_$0 : 'a my_list -> int -> int
          └──Fun acc_$0 : int -> int
             └──While true
                └──Let desugar_t1_$0
                   └──Ident l_$0 : 'a my_list
                   └──Switch
                      └──Ident desugar_t1_$0 : 'a my_list
                      └── <case>
                         └──N : tag=0
                         └──Break
                            └──Shared
                               └──Ident acc_$0 : int
                      └── <case>
                         └──C : tag=1
                         └──Shared
                            └──Let tl_$0
                               └──Get 1
                                  └──GetArg
                                     └──Ident desugar_t1_$0 : 'a my_list
                               └──Assign_Seq
                                  └── assign l_$0 : 'a my_list =
                                     └──Ident tl_$0 : 'a my_list
                                  └── assign acc_$0 : int =
                                     └──Bop + : int
                                        └──Ident acc_$0 : int
                                        └──Int 1

  |}]

let%expect_test "test tail call that requires temporaries" =
  let program =
    {|
 val rec foo = fun x -> fun y -> foo (foo x y) (foo x y)
 |}
  in
  let _ = parse_type_desugar_tco_print program in
  [%expect
    {|
    └──ValRec foo_$0
       └──Fun x_$0 : 'a -> 'a -> unit
          └──Fun y_$0 : 'a -> unit
             └──While true
                └──Let temp$0
                   └──Direct_app : foo_$0
                      └──Ident x_$0 : 'a
                      └──Ident y_$0 : 'a
                   └──Let temp$1
                      └──Direct_app : foo_$0
                         └──Ident x_$0 : 'a
                         └──Ident y_$0 : 'a
                      └──Assign_Seq
                         └── assign x_$0 : 'a =
                            └──Ident temp$0 : 'a
                         └── assign y_$0 : 'a =
                            └──Ident temp$1 : 'a |}]
