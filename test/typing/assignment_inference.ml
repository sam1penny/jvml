open Common
open Test_utils.Utils
open Test_utils

let pp_decl_result = function
  | Ok ty -> "Ok(" ^ Typing.Typed_ast.pp_texpr ty ^ ")"
  | Error _ -> "Error"

let pp_decls_result = function
  | Ok decls ->
      "Ok(\n"
      ^ (List.map
           (fun d -> Typing.Typed_ast.pp_texpr (Typing.Infer.get_decl_type d))
           decls
        |> String.concat "\n")
      ^ "\n)"
  | Error _ -> "Error"

let type_decl_and_pp d =
  Utils.add_dummy_loc_decl d |> Typing.Driver.type_decl
  >>=? (fun decl -> Ok (Typing.Infer.get_decl_type decl))
  |> pp_decl_result |> print_string

let type_progam_and_pp p =
  List.map Utils.add_dummy_loc_decl p
  |> Typing.Driver.type_program |> pp_decls_result |> print_string

let%expect_test "basic assignment" =
  let x = Val ("f", Fun ("x", Bop (Ident "x", ADD, Int 3l))) in
  type_decl_and_pp x;
  [%expect {|Ok(int -> int)|}]

let%expect_test "recursive function" =
  let x =
    ValRec
      ( "fact",
        Fun
          ( "x",
            Match
              ( Ident "x",
                [
                  (Pat_Int 1l, Int 1l);
                  ( Pat_Any,
                    Bop
                      ( Ident "x",
                        MUL,
                        App (Ident "fact", Bop (Ident "x", SUB, Int 1l)) ) );
                ] ) ) )
  in
  type_decl_and_pp x;
  [%expect {|Ok(int -> int)|}]

let%expect_test "use generic list" =
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
      Val ("x", Constr "N");
      Val ("y", App (Constr "C", Tuple [ Int 3l; Constr "N" ]));
      Val ("z", Fun ("x", App (Constr "C", Tuple [ Ident "x"; Constr "N" ])));
    ]
  in
  type_progam_and_pp x;
  [%expect {|
  Ok(
  'a list
  'a list
  int list
  'a -> 'a list
  )
|}]

let%expect_test "incorrectly use generic list" =
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
      Val
        ( "x",
          App
            ( Constr "C",
              Tuple
                [ Int 3l; App (Constr "C", Tuple [ Bool true; Constr "N" ]) ] )
        );
    ]
  in
  type_progam_and_pp x;
  [%expect {|
  Error
  |}]

let%expect_test "use generic dict" =
  let x =
    [
      Type
        ( [ "'a"; "'b" ],
          "dict",
          [
            DeclConstr ("N", None);
            DeclConstr
              ( "C",
                Some
                  (TyTuple
                     [
                       TyVar "'a";
                       TyVar "'b";
                       TyCustom ([ TyVar "'a"; TyVar "'b" ], "dict");
                     ]) );
          ] );
      Val ("x", Constr "N");
      Val ("y", App (Constr "C", Tuple [ Int 3l; Bool true; Constr "N" ]));
    ]
  in
  type_progam_and_pp x;
  [%expect {|
  Ok(
  ('a, 'b) dict
  ('a, 'b) dict
  (int, bool) dict
  )
|}]

let%expect_test "multiple usages of polymorphic function" =
  let x =
    [
      Val ("f", Fun ("x", Ident "x"));
      Val ("a", App (Ident "f", Int 3l));
      Val ("b", App (Ident "f", Bool true));
    ]
  in
  type_progam_and_pp x;
  [%expect {|
  Ok(
  'a -> 'a
  int
  bool
  )
  |}]

let%expect_test "use multiple top-level values" =
  let x =
    [
      Val ("apply", Fun ("f", Fun ("x", App (Ident "f", Ident "x"))));
      Val ("double", Fun ("x", Bop (Ident "x", MUL, Int 2l)));
      Val ("v", App (App (Ident "apply", Ident "double"), Int 3l));
    ]
  in
  type_progam_and_pp x;
  [%expect {|
  Ok(
  ('a -> 'b) -> 'a -> 'b
  int -> int
  int
  )|}]

let%expect_test "test pattern matching constructor" =
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
  type_progam_and_pp x;
  [%expect {|
    Ok(
    'a list
    ('a -> 'b) -> 'a list -> 'b list
    ) |}]

let%expect_test "variable escaping toplevel scope" =
  let x = [ Val ("x", Let ("y", Int 3l, Ident "y")); Val ("z", Ident "y") ] in
  type_progam_and_pp x;
  [%expect {|
Error
|}]

let%expect_test "test type definition with all types" =
  let x =
    [
      Type
        ( [],
          "data",
          [
            DeclConstr ("X", Some (TyFun (TyInt, TyInt)));
            DeclConstr ("Y", Some TyUnit);
            DeclConstr ("Z", Some TyBool);
            DeclConstr ("A", Some TyInt);
          ] );
      Val ("m", App (Constr "X", Fun ("x", Ident "x")));
      Val ("n", App (Constr "Y", Unit));
      Val ("o", App (Constr "Z", Bool true));
      Val ("p", App (Constr "A", Int 3l));
    ]
  in
  type_progam_and_pp x;
  [%expect {|
  Ok(
  data
  data
  data
  data
  data
  )
  |}]
