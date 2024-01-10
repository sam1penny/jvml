let () =
  let decs, instructions =
    Linear.Driver.compile_single_decl
      "val x = let apply = fun f -> fun x -> f x in apply (fun x -> x * 2) 3"
  in
  List.iter (fun x -> Jvm.Lower.lower_closure x |> print_endline) decs;
  Jvm.Lower.produce_instruction_bytecode instructions |> print_endline
