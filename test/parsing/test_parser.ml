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
          └──Br of ('a * 'b * ('a, 'b) tree * ('a, 'b) tree) |}]

let%expect_test "test function application associativity" =
  let x = "val y = (fun f -> fun x -> f x) (fun x -> x * 2) 3" in
  Parsing.Driver.parse_string x |> List.hd |> Parsing.Parsed_ast.pp_decl;
  [%expect
    {|
    └──Val y
       └──App
          └──App
             └──Fun f
                └──Fun x
                   └──App
                      └──Ident f
                      └──Ident x
             └──Fun x
                └──Bop: *
                   └──Ident x
                   └──Int 2
          └──Int 3|}]

let%expect_test "test nested patterns" =
  let x =
    "type 'a my_list = N | C of 'a * 'a my_list\n\
    \  val length_minus_one = fun x -> match x with\n\
    \  | N | C (_, N) -> 0\n\
    \  | C(_, y) -> 1 + length_minus_one y\n\
    \   "
  in
  Parsing.Driver.parse_string x
  |> List.iter (fun x ->
         Parsing.Parsed_ast.pp_decl x;
         print_newline ());
  [%expect
    {|
  └──Type my_list
     └──params = ['a]
     └──constructors
        └──N
        └──C of ('a * 'a my_list)

  └──Val length_minus_one
     └──Fun x
        └──Match
           └──Ident x
           └── <case>
              └──Pat_Or
                 └──Pat_Constr N
                 └──Pat_Constr C
                    └──Pat_Tuple
                       └──Pat_Any
                       └──Pat_Constr N
              └──Int 0
           └── <case>
              └──Pat_Constr C
                 └──Pat_Tuple
                    └──Pat_Any
                    └──Pat_Ident y
              └──Bop: +
                 └──Int 1
                 └──App
                    └──Ident length_minus_one
                    └──Ident y|}]

let%expect_test "test sequencing" =
  let x = "val x = do {1; (); true; 3}" in
  Parsing.Driver.parse_string x
  |> List.iter (fun x ->
         Parsing.Parsed_ast.pp_decl x;
         print_newline ());
  [%expect
    {|
   └──Val x
      └──Seq
         └──Int 1
         └──()
         └──Bool true
         └──Int 3 |}]

let%expect_test "test letrec without immediate binding" =
  let parse () =
    let x = "val f = let rec id = do {3; id x} in id" in
    Parsing.Driver.parse_string x
  in
  try
    let _ = parse () in
    print_endline "Ok"
  with _ ->
    print_endline "Error";
    [%expect {|Error|}]
