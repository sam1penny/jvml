let run_frontend filename =
  Parsing.Driver.parse_file filename |> Typing.Driver.type_program

let run_frontend_exn filename =
  Parsing.Driver.parse_file filename |> Typing.Infer.type_program_exn
