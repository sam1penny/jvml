let () =
  let defs, linear_ir =
    Linear.Driver.compile_single_decl "val x = let y = 3 in (fun x -> x + y) 4"
  in
  List.iter (fun c -> Linear.Instruction.show_closure c |> print_endline) defs;
  Linear.Instruction.show_program linear_ir |> print_endline
(*Jvm.Lower.produce_bytecode linear_ir |> print_endline *)
(*
let () = Driver.parse_file "examples/conditionals.jvml"
|> Interp.interp
|> Ast.string_of_const
|> print_endline
*)
