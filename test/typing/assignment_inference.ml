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

let%expect_test "basic assignment" =
  let x = Val ("f", Fun ("x", Bop (Ident "x", ADD, Int 3))) in
  Utils.add_dummy_loc_decl x |> Typing.Driver.type_decl
  >>=? (fun decl -> Ok (Typing.Infer.get_decl_type decl))
  |> pp_decl_result |> print_string;
  [%expect {|Ok(int -> int)|}]

let%expect_test "recursive function" =
  let x =
    Val
      ( "fact",
        Fun
          ( "x",
            Match
              ( Ident "x",
                [
                  (Pat_Int 1, Int 1);
                  ( Pat_Any,
                    Bop
                      ( Ident "x",
                        MUL,
                        App (Ident "fact", Bop (Ident "x", SUB, Int 1)) ) );
                ] ) ) )
  in
  Utils.add_dummy_loc_decl x |> Typing.Driver.type_decl
  >>=? (fun decl -> Ok (Typing.Infer.get_decl_type decl))
  |> pp_decl_result |> print_string;
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
      Val ("y", App (Constr "C", Tuple [ Int 3; Constr "N" ]));
      Val ("z", Fun ("x", App (Constr "C", Tuple [ Ident "x"; Constr "N" ])));
    ]
  in
  List.map Utils.add_dummy_loc_decl x
  |> Typing.Driver.type_program |> pp_decls_result |> print_string;
  [%expect {|
  Ok(
  'a list
  'b list
  int list
  'c -> 'c list
  )
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
      Val ("y", App (Constr "C", Tuple [ Int 3; Bool true; Constr "N" ]));
    ]
  in
  List.map Utils.add_dummy_loc_decl x
  |> Typing.Driver.type_program |> pp_decls_result |> print_string;
  [%expect {|
  Ok(
  ('a, 'b) dict
  ('c, 'd) dict
  (int, bool) dict
  )
|}]
