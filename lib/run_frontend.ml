let run_frontend filename =
  Parsing.Driver.parse_file filename |> Typing.Driver.type_program
