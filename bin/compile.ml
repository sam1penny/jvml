let usage_msg = "compile -s <program_string> -f <file> <options>"
let program_string = ref ""
let file = ref ""
let opt_list : string list ref = ref []
let add_opt opt () = opt_list := opt :: !opt_list

let set_opt opt =
  match opt with
  | "-opt-all" -> Common.Config.set_all_opt ()
  | "-peep" -> Common.Config.do_peephole := true
  | "-const-fp" -> Common.Config.do_constant_folding_and_prop := true
  | "-tmm" -> Common.Config.do_tail_mod_monoid := true
  | "-tco" -> Common.Config.do_tail_call_elimination := true
  | _ -> raise @@ Failure ("Unknown opt: " ^ opt)

let speclist =
  [
    ("-s", Arg.Set_string program_string, " Set string to compile");
    ("-f", Arg.Set_string file, " Set file to compile");
    ("-opt-all", Arg.Unit (add_opt "-opt-all"), " Enable all optimisations");
    ("-peep", Arg.Unit (add_opt "-peep"), " Enable peephole optimisations");
    ( "-const-fp",
      Arg.Unit (add_opt "-const-fp"),
      " Enable constant folding and propagation" );
    ("-tmm", Arg.Unit (add_opt "-tmm"), " Enable tail recursion modulo monoid");
    ("-tco", Arg.Unit (add_opt "-tco"), " Enable tail call optimisation");
  ]
  |> Arg.align

let () =
  Arg.parse speclist
    (fun _ ->
      print_endline usage_msg;
      exit 0)
    usage_msg;
  List.iter set_opt !opt_list;
  let program_text =
    if !program_string != "" then !program_string
    else if !file != "" then Jvml.Run_jvml.file_to_string !file
    else (
      print_endline usage_msg;
      exit 0)
  in
  Jvml.Run_jvml.compile_program_from_string ~filename:!file program_text
  |> print_endline
