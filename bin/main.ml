let () =
  let defs, linear_ir =
    Linear.Driver.compile_program "type either = X | Y val s = X"
  in
  List.iter
    (fun d -> Linear.Instruction.show_declaration d |> print_endline)
    defs;
  Linear.Instruction.show_program linear_ir |> print_endline
(*Jvm.Lower.produce_bytecode linear_ir |> print_endline *)
