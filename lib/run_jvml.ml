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

let run_frontend_exn_from_file filename program =
  Parsing.Driver.parse_string program
  |> Typing.Infer.type_program_exn_from_file filename
  |> Desugar.desugar_program

let run_frontend_exn_from_string program_text =
  Parsing.Driver.parse_string program_text
  |> Typing.Infer.type_program_exn_from_string program_text
  |> Desugar.desugar_program

let linear_ir_from_string program_text =
  run_frontend_exn_from_string program_text
  |> Middle_end.Driver.run_middleend |> Linearise.Driver.lower_program_to_linear_ir
  |> Linearise.Instruction.show_program

let run_backend typed_tree =
  Middle_end.Driver.run_middleend typed_tree
  |> Linearise.Driver.lower_program_to_linear_ir |> Jvm.Driver.lower_ir

let compile_program_from_string program_text =
  run_frontend_exn_from_string program_text |> run_backend

let compile_program_from_file filename =
  let program_text = file_to_string filename in
  run_frontend_exn_from_file filename program_text |> run_backend
