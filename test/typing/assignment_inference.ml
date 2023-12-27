open Common
open Test_utils.Utils
open Test_utils

let pp_decl_result = function
  | Ok ty -> "Ok(" ^ pp_texpr ty ^ ")"
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
