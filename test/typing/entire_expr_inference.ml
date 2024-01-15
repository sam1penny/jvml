(* rather than just looking at the inferred type, we check the entire tree*)

open Test_utils.Utils
open Test_utils

let pp_entire_tree_result = function
  | Ok tt ->
      print_endline "Ok(";
      Typing.Typed_ast.pp_expr tt;
      print_endline ")"
  | Error _ -> print_endline "Error"

let%expect_test "test nested function unification + simplification in APPLY" =
  let x = Fun ("f", Fun ("x", App (Ident "f", Ident "x"))) in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr |> pp_entire_tree_result;
  [%expect
    {|
    Ok(
    └──Fun f : ('a -> 'b) -> 'a -> 'b
       └──Fun x : 'a -> 'b
          └──App
             └──Ident f : 'a -> 'b
             └──Ident x : 'a
    )|}]

let%expect_test "test nested tuple unification + simplification" =
  let x =
    Fun
      ( "x",
        Fun ("y", Tuple [ Ident "x"; Ident "y"; Tuple [ Ident "x"; Ident "x" ] ])
      )
  in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr |> pp_entire_tree_result;
  [%expect
    {|
    Ok(
    └──Fun x : 'a -> 'b -> ('a * 'b * ('a * 'a))
       └──Fun y : 'b -> ('a * 'b * ('a * 'a))
          └──Tuple : ('a * 'b * ('a * 'a))
             └──Ident x : 'a
             └──Ident y : 'b
             └──Tuple : ('a * 'a)
                └──Ident x : 'a
                └──Ident x : 'a
    )
  |}]

let%expect_test "test operator unification + simplification" =
  let x = Fun ("x", Bop (Ident "x", EQ, Bop (Ident "x", MUL, Ident "x"))) in
  Utils.add_dummy_loc_expr x |> Typing.Driver.type_expr |> pp_entire_tree_result;
  [%expect
    {|
    Ok(
    └──Fun x : int -> bool
       └──Bop = : bool
          └──Ident x : int
          └──Bop * : int
             └──Ident x : int
             └──Ident x : int
    )
  |}]
