let () =
  Parsing.Driver.parse_file "examples/typeparsing.jvml"
  |> List.iter Parsing.Parsed_ast.pp_decl
(*
  Run_frontend.run_frontend "examples/everything.jvml"
  |> List.iter (fun d -> Parsing.Parsed_ast.pp_decl d |> print_endline) *)

(*
let () = Driver.parse_file "examples/conditionals.jvml"
|> Interp.interp
|> Ast.string_of_const
|> print_endline
*)
