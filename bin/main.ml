let () =
  match Jvml.Run_frontend.run_frontend "examples/typeparsing.jvml" with
  | Ok typed_decls -> List.iter Typing.Typed_ast.pp_decl typed_decls
  | Error e -> raise @@ Invalid_argument e

(*
let () = Driver.parse_file "examples/conditionals.jvml"
|> Interp.interp
|> Ast.string_of_const
|> print_endline
*)
