let file_to_string filename =
  let ch = In_channel.open_bin filename in
  let s =
    In_channel.really_input_string ch (In_channel.length ch |> Int64.to_int)
  in
  In_channel.close ch;
  Option.get s

let run_frontend filename =
  Parsing.Driver.parse_string filename
  |> Common.maybe_record_compiler_time "parsing"
  |> Typing.Driver.type_program
  |> Common.maybe_record_compiler_time "typing"
  |> fun p ->
  Result.bind p (fun program -> Ok (Desugar.desugar_program program))

let run_frontend_exn_from_file filename program =
  Parsing.Driver.parse_string program
  |> Common.maybe_record_compiler_time "parsing"
  |> Typing.Infer.type_program_exn_from_file filename
  |> Common.maybe_record_compiler_time "typing"
  |> Desugar.desugar_program
  |> Common.maybe_record_compiler_time "desugaring"

let run_frontend_exn_from_string program_text =
  Parsing.Driver.parse_string program_text
  |> Common.maybe_record_compiler_time "parsing"
  |> Typing.Infer.type_program_exn_from_string program_text
  |> Common.maybe_record_compiler_time "typing"
  |> Desugar.desugar_program
  |> Common.maybe_record_compiler_time "desugaring"

let linear_ir_from_string program_text =
  run_frontend_exn_from_string program_text
  |> Middle_end.Driver.run_middleend
  |> Common.maybe_record_compiler_time "middle_end"
  |> Linearise.Driver.lower_program_to_linear_ir
  |> Common.maybe_record_compiler_time "linearise"
  |> Linearise.Instruction.show_program

let run_backend typed_tree =
  Middle_end.Driver.run_middleend typed_tree
  |> Common.maybe_record_compiler_time "middle_end"
  |> Linearise.Driver.lower_program_to_linear_ir
  |> Common.maybe_record_compiler_time "linearise"
  |> Jvm.Driver.lower_ir
  |> Common.maybe_record_compiler_time "lower_jvm"

let compile_program_from_string program_text =
  Common.maybe_record_compiler_time "startup" program_text
  |> run_frontend_exn_from_string |> run_backend

let compile_program_from_file filename =
  Common.maybe_record_compiler_time "startup" filename
  |> file_to_string
  |> run_frontend_exn_from_file filename
  |> run_backend
