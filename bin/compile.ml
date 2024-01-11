let usage_msg = "compile -s <program_string> -f <file>"
let program_string = ref ""
let file = ref ""

let speclist =
  [
    ("-s", Arg.Set_string program_string, "Set string to compile");
    ("-f", Arg.Set_string file, "Set file to compile");
  ]

let open_file filename =
  let ch = In_channel.open_bin filename in
  let s =
    In_channel.really_input_string ch (In_channel.length ch |> Int64.to_int)
  in
  In_channel.close ch;
  Option.get s

let () =
  Arg.parse speclist
    (fun _ ->
      print_endline usage_msg;
      exit 0)
    usage_msg;
  let prog =
    if !program_string != "" then !program_string
    else if !file != "" then open_file !file
    else (
      print_endline usage_msg;
      exit 0)
  in
  let decs, instructions = Linear.Driver.compile_program prog in
  print_endline Jvm.Lower.stdlib;
  List.iter (fun x -> Jvm.Lower.lower_closure x |> print_endline) decs;
  Jvm.Lower.produce_instruction_bytecode instructions |> print_endline
