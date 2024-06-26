(* tests for inferring standalone expression types *)
open Common
open Test_utils.Utils
open Test_utils

let%expect_test "basic int arithmetic" =
  let x = Bop (Int 6l, MUL, Bop (Int 3l, ADD, Int 4l)) in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Ok(int)|}]

let%expect_test "invalid arithmetic" =
  let x = Bop (Int 6l, MUL, Bool false) in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Error|}]

let%expect_test "unbound variable" =
  let x = Ident "x" in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Error|}]

let%expect_test "basic if statement" =
  let x = If (Bool true, Unit, Unit) in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Ok(unit)|}]

let%expect_test "infer simple function argument" =
  let x = Fun ("x", Bop (Ident "x", SUB, Int 4l)) in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Ok(int -> int)|}]

let%expect_test "infer polymorphic function application" =
  let x = Fun ("f", Fun ("x", App (Ident "f", Ident "x"))) in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Ok(('a -> 'b) -> 'a -> 'b)|}]

let%expect_test "function variable shadowing" =
  (* fun x -> ((fun x -> 3 + x), x && true)*)
  let x =
    Fun
      ( "x",
        Tuple
          [
            Fun ("x", Bop (Int 3l, ADD, Ident "x"));
            Bop (Ident "x", AND, Bool true);
          ] )
  in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Ok(bool -> ((int -> int) * bool))|}]

let%expect_test "function variable shadowing - reversed" =
  (* fun x -> (x && true, (fun x -> 3 + x))*)
  let x =
    Fun
      ( "x",
        Tuple
          [
            Bop (Ident "x", AND, Bool true);
            Fun ("x", Bop (Int 3l, ADD, Ident "x"));
          ] )
  in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Ok(bool -> (bool * (int -> int)))|}]

let%expect_test "infer basic let expression" =
  let x = Let ("x", Int 3l, Ident "x") in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Ok(int)|}]

let%expect_test "test let polymorphism" =
  (* let f = fun x -> x in (f 3, f true)*)
  let x =
    Let
      ( "f",
        Fun ("x", Ident "x"),
        Tuple [ App (Ident "f", Int 3l); App (Ident "f", Bool true) ] )
  in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Ok((int * bool))|}]

let%expect_test "test nested let" =
  let x = Let ("x", Int 3l, Let ("y", Ident "x", Ident "y")) in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Ok(int)|}]

let%expect_test "test binding match" =
  let x = Match (Int 3l, [ (Pat_Ident "x", Ident "x") ]) in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Ok(int)|}]

let%expect_test "test shadowing binding match" =
  (* fun x -> (x && true, match 3 with x -> x)*)
  let x =
    Fun
      ( "x",
        Tuple
          [
            Bop (Ident "x", AND, Bool true);
            Match (Int 3l, [ (Pat_Ident "x", Ident "x") ]);
          ] )
  in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Ok(bool -> (bool * int))|}]

let%expect_test "test matching tuple" =
  let x =
    Fun
      ( "x",
        Match
          ( Ident "x",
            [
              ( Pat_Tuple [ Pat_Ident "x"; Pat_Ident "y" ],
                Tuple [ Ident "x"; Ident "y" ] );
            ] ) )
  in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Ok(('a * 'b) -> ('a * 'b))|}]

let%expect_test "test pattern with duplicate bindings" =
  let x =
    Match
      ( Tuple [ Int 0l; Int 1l ],
        [ (Pat_Tuple [ Pat_Ident "x"; Pat_Ident "x" ], Int 3l) ] )
  in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Error|}]

let%expect_test "test valid polymorphic equals" =
  let x = Tuple [ Bop (Int 3l, EQ, Int 3l); Bop (Bool true, EQ, Bool false) ] in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Ok((bool * bool))|}]

let%expect_test "test invalid polymorphic equals" =
  let x = Bop (Int 3l, EQ, Bool true) in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Error|}]

let%expect_test "test ored pattern" =
  let x = Match (Int 3l, [ (Pat_Or [ Pat_Int 0l; Pat_Int 1l ], Int 1l) ]) in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Ok(int)|}]

let%expect_test "test function equality fails" =
  let x = Bop (Fun ("x", Ident "x"), EQ, Fun ("y", Ident "y")) in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Error|}]

let%expect_test "test sequence typing" =
  let x = Seq [ Unit; Bool true; Int 3l ] in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Ok(int)|}]

let%expect_test "test matched pattern bindings ok" =
  let x =
    Fun
      ( "x",
        Match
          ( Ident "x",
            [
              ( Pat_Or
                  [
                    Pat_Tuple [ Pat_Int 3l; Pat_Ident "x" ];
                    Pat_Tuple [ Pat_Ident "x"; Pat_Int 3l ];
                  ],
                Bool true );
            ] ) )
  in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Ok((int * int) -> bool)|}]

let%expect_test "test uncorresponding types in pattern bindings fails" =
  let x =
    Fun
      ( "x",
        Match
          ( Ident "x",
            [
              ( Pat_Or
                  [
                    Pat_Tuple [ Pat_Int 3l; Pat_Ident "x" ];
                    Pat_Tuple [ Pat_Ident "x"; Pat_Bool true ];
                  ],
                Bool true );
            ] ) )
  in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Error|}]

let%expect_test "test unmatched pattern bindings fails" =
  let x =
    Fun
      ( "x",
        Match
          (Ident "x", [ (Pat_Or [ Pat_Ident "x"; Pat_Ident "y" ], Bool true) ])
      )
  in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Error|}]

let%expect_test "test let rec function typing" =
  let x = LetRec ("x", Fun ("x", Ident "x"), Ident "x") in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr
  |> Result.map Typing.Infer.get_expr_type
  |> pp_tree_result |> print_string;
  [%expect {|Ok('a -> 'a)|}]
