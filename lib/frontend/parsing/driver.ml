Jvml_parser.Wrapped_parser.pp_exceptions ()

let parse_file filename =
  Jvml_parser.Wrapped_parser.parse_file filename
  |> List.map (Jvml_parser.map_over_decl_exprs Jvml_parser.check_letrec)
  |> List.map Jvml_parser.check_valrec

let parse_string s =
  Jvml_parser.Wrapped_parser.parse_string s
  |> List.map (Jvml_parser.map_over_decl_exprs Jvml_parser.check_letrec)
  |> List.map Jvml_parser.check_valrec
