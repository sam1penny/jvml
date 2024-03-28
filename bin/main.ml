let () =
  let string_program =
    {|
    val x = let apply = fun f -> fun x -> f x in print(apply (fun x -> 2 * x) 3)
    |}
  in
  Common.Config.set_all_opt ();
  Parsing.Driver.parse_string string_program
  |> Typing.Infer.type_program_exn_from_string "test_env"
  |> Desugar.desugar_program |> Middle_end.Driver.run_middleend
  |> List.iter Desugar.Desugared_ast.pp_decl
(*
  |> Middle_end.Direct_calls.transform_direct_call_program
  |> fun p ->
  List.iter Desugar.Desugared_ast.pp_decl p; p
  |> fun t ->
   (Middle_end.Inline.occurrence_analysis t
   |> Hashtbl.iter (fun k v -> Printf.printf "%s %s\n" k (Middle_end.Inline.show_binding_occurrence v))); t
  |> Middle_end.Inline.inline_program
  |> List.iter Desugar.Desugared_ast.pp_decl
*)
