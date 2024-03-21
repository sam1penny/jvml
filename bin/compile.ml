let usage_msg = "compile -s <program_string> -f <file>"
let program_string = ref ""
let file = ref ""

let speclist =
  [
    ("-s", Arg.Set_string program_string, "Set string to compile");
    ("-f", Arg.Set_string file, "Set file to compile");
  ]

let () =
  Arg.parse speclist
    (fun _ ->
      print_endline usage_msg;
      exit 0)
    usage_msg;
  let program_text =
    if !program_string != "" then !program_string
    else if !file != "" then Jvml.Run_jvml.file_to_string !file
    else (
      print_endline usage_msg;
      exit 0)
  in
  Jvml.Run_jvml.compile_program_from_string ~filename:!file program_text
  |> print_endline
