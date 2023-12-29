let () =
  let typed_decls =
    Jvml.Run_frontend.run_frontend_exn "examples/typeparsing.jvml"
  in
  List.iter Typing.Typed_ast.pp_decl typed_decls;
  List.map Typing.Infer.get_decl_type typed_decls
  |> List.iter (fun t -> print_endline (Typing.Typed_ast.pp_texpr t))
(*
  match Jvml.Run_frontend.run_frontend "examples/typeparsing.jvml" with
  | Ok typed_decls ->
      List.iter Typing.Typed_ast.pp_decl typed_decls;
      List.map Typing.Infer.get_decl_type typed_decls
      |> List.iter (fun t -> print_endline (Common.pp_texpr t))
  | Error (_, e) -> raise @@ Invalid_argument e
  *)

(*
let () = Driver.parse_file "examples/conditionals.jvml"
|> Interp.interp
|> Ast.string_of_const
|> print_endline
*)
