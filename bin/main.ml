let () =
  let string_program =
    {|
    type nat = Z | S of nat
    val lt_3 = fun x -> match x with
      Z | S (S Z | Z) -> true
      | _ -> false
    |}
  in
  Parsing.Driver.parse_string string_program
  |> Typing.Infer.type_program_exn_from_string string_program
  |> Desugar.desugar_program
  |> List.map Desugar.Desugared_ast.pp_decl
  |> List.hd
(* |> Desugar.desugar_program |> Linear.Driver.lower_program_to_linear_ir
   |> Linear.Instruction.show_program |> print_endline
*)
