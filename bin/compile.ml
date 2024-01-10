let () =
  let decs, instructions =
    Linear.Driver.compile_single_decl
      "val x = let y = 3 in 10 + ((fun x -> x + y) 5)"
  in
  Jvm.Lower.lower_closure (List.hd decs) |> print_endline;
  Jvm.Lower.produce_instruction_bytecode instructions |> print_endline
