let file_to_string filename =
  let ch = In_channel.open_bin filename in
  let s =
    In_channel.really_input_string ch (In_channel.length ch |> Int64.to_int)
  in
  In_channel.close ch;
  Option.get s

let run_frontend filename =
  Parsing.Driver.parse_string filename |> Typing.Driver.type_program |> fun p ->
  Result.bind p (fun program -> Ok (Desugar.desugar_program program))

let run_frontend_exn filename program =
  Parsing.Driver.parse_string program
  |> Typing.Infer.type_program_exn_from_file filename
  |> Desugar.desugar_program

let linear_ir_from_string filename program =
  run_frontend_exn filename program
  |> Middle_end.Driver.run_middleend |> Linear.Driver.lower_program_to_linear_ir
  |> Linear.Instruction.show_program

let compile_program_from_string ?(filename = "no_file") program =
  run_frontend_exn filename program
  |> Middle_end.Driver.run_middleend |> Linear.Driver.lower_program_to_linear_ir
  |> Jvm.Driver.lower_ir

let compile_program_from_file file =
  let program_text = file_to_string file in
  compile_program_from_string ~filename:file program_text
