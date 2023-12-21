(* tests for inferring standalone expression types *)
open Parsing.Parsed_ast
open Common

let pp_tree_result = function
  | Ok ty -> "Ok(" ^ Typing.Typed_ast.ty_repr ty ^ ")"
  | Error _ -> "Error"

let%expect_test "basic int arithmetic" =
  let x = Oper (Int 6, MUL, Oper (Int 3, ADD, Int 4)) in
  Typing.Driver.type_tree x |> pp_tree_result |> print_string;
  [%expect {|Ok(int)|}]

let%expect_test "invalid arithmetic" =
  let x = Oper (Int 6, MUL, Bool false) in
  Typing.Driver.type_tree x |> pp_tree_result |> print_string;
  [%expect {|Error|}]

let%expect_test "unbound variable" =
  let x = Ident "x" in
  Typing.Driver.type_tree x |> pp_tree_result |> print_string;
  [%expect {|Error|}]

let%expect_test "basic if statement" =
  let x = If (Bool true, Unit, Unit) in
  Typing.Driver.type_tree x |> pp_tree_result |> print_string;
  [%expect {|Ok(unit)|}]

let%expect_test "infer simple function argument" =
  let x = Fun ("x", Oper (Ident "x", SUB, Int 4)) in
  Typing.Driver.type_tree x |> pp_tree_result |> print_string;
  [%expect {|Ok(int -> int)|}]

let%expect_test "infer polymorphic function application" =
  let x = Fun ("f", Fun ("x", App (Ident "f", Ident "x"))) in
  Typing.Driver.type_tree x |> pp_tree_result |> print_string;
  [%expect {|Ok(('a -> 'b) -> 'a -> 'b)|}]

let%expect_test "function variable shadowing" =
  (* fun x -> ((fun x -> 3 + x), x && true)*)
  let x =
    Fun
      ( "x",
        Tuple
          [
            Fun ("x", Oper (Int 3, ADD, Ident "x"));
            Oper (Ident "x", AND, Bool true);
          ] )
  in
  Typing.Driver.type_tree x |> pp_tree_result |> print_string;
  [%expect {|Ok(bool -> ((int -> int) * bool))|}]

let%expect_test "function variable shadowing - reversed" =
  (* fun x -> (x && true, (fun x -> 3 + x))*)
  let x =
    Fun
      ( "x",
        Tuple
          [
            Oper (Ident "x", AND, Bool true);
            Fun ("x", Oper (Int 3, ADD, Ident "x"));
          ] )
  in
  Typing.Driver.type_tree x |> pp_tree_result |> print_string;
  [%expect {|Ok(bool -> (bool * (int -> int)))|}]

let%expect_test "infer basic let expression" =
  let x = Let ("x", Int 3, Ident "x") in
  Typing.Driver.type_tree x |> pp_tree_result |> print_string;
  [%expect {|Ok(int)|}]

let%expect_test "test let polymorphism" =
  (* let f = fun x -> x in (f 3, f true)*)
  let x =
    Let
      ( "f",
        Fun ("x", Ident "x"),
        Tuple [ App (Ident "f", Int 3); App (Ident "f", Bool true) ] )
  in
  Typing.Driver.type_tree x |> pp_tree_result |> print_string;
  [%expect {|Ok((int * bool))|}]

let%expect_test "test nested let" =
  let x = Let ("x", Int 3, Let ("y", Ident "x", Ident "y")) in
  Typing.Driver.type_tree x |> pp_tree_result |> print_string;
  [%expect {|Ok(int)|}]

let%expect_test "test binding match" =
  let x = Match (Int 3, [ (Pat_Ident "x", Ident "x") ]) in
  Typing.Driver.type_tree x |> pp_tree_result |> print_string;
  [%expect {|Ok(int)|}]

let%expect_test "test shadowing binding match" =
  (* fun x -> (x && true, match 3 with x -> x)*)
  let x =
    Fun
      ( "x",
        Tuple
          [
            Oper (Ident "x", AND, Bool true);
            Match (Int 3, [ (Pat_Ident "x", Ident "x") ]);
          ] )
  in
  Typing.Driver.type_tree x |> pp_tree_result |> print_string;
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
  Typing.Driver.type_tree x |> pp_tree_result |> print_string;
  [%expect {|Ok(('a * 'b) -> ('a * 'b))|}]

let%expect_test "test pattern with duplicate bindings" =
  let x =
    Match
      ( Tuple [ Int 0; Int 1 ],
        [ (Pat_Tuple [ Pat_Ident "x"; Pat_Ident "y" ], Int 3) ] )
  in
  Typing.Driver.type_tree x |> pp_tree_result |> print_string;
  [%expect {|Error|}]
