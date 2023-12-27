let%expect_test "test arithmetic precedence" =
  let x = "val x = 3 + 5 * 3 * (0 - 1)" in
  Parsing.Driver.parse_string x |> List.hd |> Parsing.Parsed_ast.pp_decl;
  [%expect
    {|
    └──Val x
       └──Bop: +
          └──Int 3
          └──Bop: *
             └──Bop: *
                └──Int 5
                └──Int 3
             └──Bop: -
                └──Int 0
                └──Int 1|}]

let%expect_test "multiple patterns same expr" =
  let x = "val x = match 3 with 0 | 1 -> true | _ -> false" in
  Parsing.Driver.parse_string x |> List.hd |> Parsing.Parsed_ast.pp_decl;
  [%expect
    {|
    └──Val x
       └──Match
          └──Int 3
          └── <case>
             └──Pat_Or
                └──Pat_Int 0
                └──Pat_Int 1
             └──Bool true
          └── <case>
             └──Pat_Any
             └──Bool false |}]

let%expect_test "test factorial program" =
  let x = "val fact = fun x -> if x = 0 then 1 else x * fact (n - 1)" in
  Parsing.Driver.parse_string x |> List.hd |> Parsing.Parsed_ast.pp_decl;
  [%expect
    {|
    └──Val fact
       └──Fun x
          └──If
             └──Bop: =
                └──Ident x
                └──Int 0
             └──Int 1
             └──Bop: *
                └──Ident x
                └──App
                   └──Ident fact
                   └──Bop: -
                      └──Ident n
                      └──Int 1 |}]

let%expect_test "test type definitions" =
  let x =
    "type ('a, 'b) tree = Lf | Br of ('a * 'b * ('a, 'b) tree * ('a, 'b) tree)"
  in
  Parsing.Driver.parse_string x |> List.hd |> Parsing.Parsed_ast.pp_decl;
  [%expect
    {|
    └──Type tree
       └──params = ['a,'b]
       └──constructors
          └──Lf
          └──Br of (a * b * (a, b) tree * (a, b) tree) |}]
