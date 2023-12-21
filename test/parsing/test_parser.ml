(*open Jvml

  let%expect_test _ =
    let x = Ast.ADD (Ast.INT 3, Ast.INT 4) in
    Ast.pretty_print x;
    [%expect{|
      ---- 3
       +
      ---- 4
    |}]
*)
