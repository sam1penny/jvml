let () =
  let string_program =
    "val recfun = let rec innerrec = fun x -> if x = 0 then 1 else 1 + \
     innerrec(x - 1) in innerrec val z = print(recfun 10)"
  in
  Parsing.Driver.parse_string string_program
  |> Typing.Infer.type_program_exn_from_string string_program
  |> List.hd |> Typing.Infer.get_decl_type |> Typing.Typed_ast.pp_texpr
  |> print_endline
