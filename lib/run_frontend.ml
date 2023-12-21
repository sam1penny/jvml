let run_frontend filename =
  match Parsing.Driver.parse_file filename |> List.hd with
  | Parsing.Parsed_ast.Val (_, e) -> Typing.Driver.type_tree e
  | _ -> raise @@ Invalid_argument "unsupported more than expressions"
