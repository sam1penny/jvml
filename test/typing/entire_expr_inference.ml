(* rather than just looking at the inferred type, we check the entire tree*)

open Test_utils.Utils
open Test_utils

let pp_entire_tree_result = function
  | Ok tt ->
      print_endline "Ok(";
      Typing.Typed_ast.pp_expr tt;
      print_endline ")"
  | Error _ -> print_endline "Error"

let pp_entire_decl_result = function
  | Ok decl ->
      print_endline "Ok(";
      Typing.Typed_ast.pp_decl decl;
      print_endline ")"
  | Error _ -> print_endline "Error"

let pp_entire_progam_result = function
  | Ok program ->
      print_endline "Ok(";
      List.iter Typing.Typed_ast.pp_decl program;
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

let%expect_test "test valid type definition" =
  let x =
    Type
      ( [ "'a" ],
        "list",
        [
          DeclConstr ("Nil", None);
          DeclConstr
            ("Some", Some (TyTuple [ TyInt; TyCustom ([ TyVar "'a" ], "list") ]));
        ] )
  in
  Utils.add_dummy_loc_decl x |> Typing.Driver.type_decl |> pp_entire_decl_result;
  [%expect
    {|
    Ok(
    └──Type list
       └──params = ['a]
       └──constructors
          └──Nil
          └──Some of (int * 'a list)
    )|}]

let%expect_test "test invalid type definition - unknown parameter" =
  let x =
    Type
      ( [ "'a" ],
        "list",
        [
          DeclConstr ("Nil", None);
          DeclConstr
            ("Some", Some (TyTuple [ TyInt; TyCustom ([ TyVar "'b" ], "list") ]));
        ] )
  in
  Utils.add_dummy_loc_decl x |> Typing.Driver.type_decl |> pp_entire_decl_result;
  [%expect {|Error|}]

let%expect_test "test entire tree with pattern matching constructor" =
  let x =
    [
      Type
        ( [ "'a" ],
          "list",
          [
            DeclConstr ("N", None);
            DeclConstr
              ( "C",
                Some (TyTuple [ TyVar "'a"; TyCustom ([ TyVar "'a" ], "list") ])
              );
          ] );
      ValRec
        ( "map",
          Fun
            ( "f",
              Fun
                ( "x",
                  Match
                    ( Ident "x",
                      [
                        (Pat_Constr ("N", None), Constr "N");
                        ( Pat_Constr
                            ( "C",
                              Some
                                (Pat_Tuple [ Pat_Ident "hd"; Pat_Ident "tl" ])
                            ),
                          App
                            ( Constr "C",
                              Tuple
                                [
                                  App (Ident "f", Ident "hd");
                                  App (App (Ident "map", Ident "f"), Ident "tl");
                                ] ) );
                      ] ) ) ) );
    ]
  in
  List.map Utils.add_dummy_loc_decl x
  |> Typing.Driver.type_program |> pp_entire_progam_result;
  [%expect
    {|
    Ok(
    └──Type list
       └──params = ['a]
       └──constructors
          └──N
          └──C of ('a * 'a list)
    └──ValRec map
       └──Fun f : ('a -> 'b) -> 'a list -> 'b list
          └──Fun x : 'a list -> 'b list
             └──Match
                └──Ident x : 'a list
                └── <case>
                   └──Pat_Constr N : 'a list
                   └──Constructor N : 'b list
                └── <case>
                   └──Pat_Constr C : 'a list
                      └──Pat_Tuple
                         └──Pat_Ident hd : 'a
                         └──Pat_Ident tl : 'a list
                   └──App
                      └──Constructor C : ('b * 'b list) -> 'b list
                      └──Tuple : ('b * 'b list)
                         └──App
                            └──Ident f : 'a -> 'b
                            └──Ident hd : 'a
                         └──App
                            └──App
                               └──Ident map : ('a -> 'b) -> 'a list -> 'b list
                               └──Ident f : 'a -> 'b
                            └──Ident tl : 'a list
    ) |}]
