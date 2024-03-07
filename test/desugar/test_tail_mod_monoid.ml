let run s =
  Parsing.Driver.parse_string s
  |> Typing.Infer.type_program_exn_from_string "test_env"
  |> Desugar.desugar_program
  |> Middle_end.Direct_calls.transform_direct_call_program
  |> Middle_end.Tail_mod_monoid.transform_tmm_program
  |> List.iter Desugar.Desugared_ast.pp_decl

let%expect_test "test simple recursive sum" =
  let program =
    {|
  val rec sum = fun n ->
    if n = 0 then 0
    else n + sum (n - 1)
  |}
  in
  let _ = run program in
  [%expect
    {|
    └──ValRec sum_$0_acc
       └──Fun acc : int -> int -> int
          └──Fun n_$0 : int -> int
             └──If
                └──Bop = : bool
                   └──Ident n_$0 : int
                   └──Int 0
                └──Bop + : int
                   └──Ident acc : int
                   └──Int 0
                └──Direct_app : sum_$0_acc
                   └──Bop + : int
                      └──Ident acc : int
                      └──Ident n_$0 : int
                   └──Bop - : int
                      └──Ident n_$0 : int
                      └──Int 1
    └──ValRec sum_$0
       └──Fun n_$0 : int -> int
          └──Direct_app : sum_$0_acc
             └──Int 0
             └──Ident n_$0 : int |}]

let%expect_test "test tmm with tail call" =
  let program =
    {|
  val rec foo = fun n ->
  if n = 0 then 0
  else if n = 1 then foo (n - 1)
  else n + foo (n - 1)
  |}
  in
  let _ = run program in
  [%expect
    {|
  └──ValRec foo_$0_acc
     └──Fun acc : int -> int -> int
        └──Fun n_$0 : int -> int
           └──If
              └──Bop = : bool
                 └──Ident n_$0 : int
                 └──Int 0
              └──Bop + : int
                 └──Ident acc : int
                 └──Int 0
              └──If
                 └──Bop = : bool
                    └──Ident n_$0 : int
                    └──Int 1
                 └──Direct_app : foo_$0_acc
                    └──Ident acc : int
                    └──Bop - : int
                       └──Ident n_$0 : int
                       └──Int 1
                 └──Direct_app : foo_$0_acc
                    └──Bop + : int
                       └──Ident acc : int
                       └──Ident n_$0 : int
                    └──Bop - : int
                       └──Ident n_$0 : int
                       └──Int 1
  └──ValRec foo_$0
     └──Fun n_$0 : int -> int
        └──Direct_app : foo_$0_acc
           └──Int 0
           └──Ident n_$0 : int |}]

let%expect_test "test non-direct application provided acc argument" =
  let program =
    {|
  val rec foo = fun n ->
  if n = 0 then let alias = foo in alias 3
  else n + foo (n - 1)
  |}
  in
  let _ = run program in
  [%expect
    {|
    └──ValRec foo_$0_acc
       └──Fun acc : int -> int -> int
          └──Fun n_$0 : int -> int
             └──If
                └──Bop = : bool
                   └──Ident n_$0 : int
                   └──Int 0
                └──Let alias_$0
                   └──App
                      └──Ident foo_$0_acc : int -> int -> int
                      └──Int 0
                   └──Bop + : int
                      └──Ident acc : int
                      └──App
                         └──Ident alias_$0 : int -> int
                         └──Int 3
                └──Direct_app : foo_$0_acc
                   └──Bop + : int
                      └──Ident acc : int
                      └──Ident n_$0 : int
                   └──Bop - : int
                      └──Ident n_$0 : int
                      └──Int 1
    └──ValRec foo_$0
       └──Fun n_$0 : int -> int
          └──Direct_app : foo_$0_acc
             └──Int 0
             └──Ident n_$0 : int |}]

let%expect_test "test one call suitable for trmc, one not" =
  let program =
    {|
  val rec foo = fun n ->
  if n = 0 then 1
  else foo(n - 2) + foo (n - 1)
  |}
  in
  let _ = run program in
  [%expect
    {|
    └──ValRec foo_$0_acc
       └──Fun acc : int -> int -> int
          └──Fun n_$0 : int -> int
             └──If
                └──Bop = : bool
                   └──Ident n_$0 : int
                   └──Int 0
                └──Bop + : int
                   └──Ident acc : int
                   └──Int 1
                └──Direct_app : foo_$0_acc
                   └──Direct_app : foo_$0_acc
                      └──Ident acc : int
                      └──Bop - : int
                         └──Ident n_$0 : int
                         └──Int 2
                   └──Bop - : int
                      └──Ident n_$0 : int
                      └──Int 1
    └──ValRec foo_$0
       └──Fun n_$0 : int -> int
          └──Direct_app : foo_$0_acc
             └──Int 0
             └──Ident n_$0 : int |}]
