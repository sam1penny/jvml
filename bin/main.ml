let () =
  let string_program =
    {|
    val rec foo = fun n ->
      if n = 0 then 1
      else if n = 1 then (fun x -> x) (foo 3)
      else foo(n - 2) + foo (n - 1)
  |}
  in
  Parsing.Driver.parse_string string_program
  |> Typing.Infer.type_program_exn_from_string "test_env"
  |> Desugar.desugar_program
  |> Middle_end.Direct_calls.transform_direct_call_program
  |> Middle_end.Tail_mod_monoid.transform_tmm_program
  |> List.iter Desugar.Desugared_ast.pp_decl

(*
let string_program =
    {|
    val run_quad =
      let double = fun x -> x * 2 in
      let quad = fun y -> double y * double y in
      quad 4
    val test = print(run_quad)
    |}
  in
  Parsing.Driver.parse_string string_program
  |> Typing.Infer.type_program_exn_from_string "test_env"
  (*|> Desugar.desugar_program
    |> List.map Desugar.Desugared_ast.pp_decl
    |> List.hd
  *)
  |> Desugar.desugar_program
  |> List.iter Desugar.Desugared_ast.pp_decl
*)
