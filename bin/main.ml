let () =
  match Jvml.Run_frontend.run_frontend "examples/typeparsing.jvml" with
  | Ok typed_decls ->
      List.iter Typing.Typed_ast.pp_decl typed_decls;
      List.map Typing.Infer.get_decl_type typed_decls
      |> List.iter (fun t -> print_endline (Common.pp_texpr t))
  | Error e -> raise @@ Invalid_argument e

(*
let () = Driver.parse_file "examples/conditionals.jvml"
|> Interp.interp
|> Ast.string_of_const
|> print_endline
*)
