open Jvml

let () = Driver.parse_file "examples/everything.jvml"
|> List.iter (fun d -> Ast.pp_decl d |> print_endline)


(*
let () = Driver.parse_file "examples/conditionals.jvml"
|> Interp.interp
|> Ast.string_of_const
|> print_endline
*)