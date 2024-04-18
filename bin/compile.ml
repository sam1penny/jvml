let usage_msg = "compile -s <program_string> -f <file> <options>"

type program_input = String of string | File of string

let program_input = ref None
let opt_list : string list ref = ref []
let add_opt opt () = opt_list := opt :: !opt_list

let set_opt opt =
  match opt with
  | "-opt-all" -> Common.Config.set_all_opt ()
  | "-peep" -> Common.Config.do_peephole := true
  | "-const-fp" -> Common.Config.do_constant_folding_and_prop := true
  | "-tmm" -> Common.Config.do_tail_mod_monoid := true
  | "-tco" -> Common.Config.do_tail_call_elimination := true
  | "-inline" -> Common.Config.do_inlining := true
  | "-tmc" -> Common.Config.do_tail_mod_cons := true
  | _ -> raise @@ Failure ("Unknown opt: " ^ opt)

let speclist =
  [
    ( "-s",
      Arg.String (fun x -> program_input := Some (String x)),
      " Set string to compile" );
    ( "-f",
      Arg.String (fun x -> program_input := Some (File x)),
      " Set file to compile" );
    ( "-o",
      Arg.Set_string Common.Config.generated_class_name,
      " Set name of generated class" );
    ("-opt-all", Arg.Unit (add_opt "-opt-all"), " Enable all optimisations");
    ("-peep", Arg.Unit (add_opt "-peep"), " Enable peephole optimisations");
    ( "-const-fp",
      Arg.Unit (add_opt "-const-fp"),
      " Enable constant folding and propagation" );
    ("-inline", Arg.Unit (add_opt "-inline"), "Enable inlining");
    ( "-inl-threshold",
      Arg.Set_int Common.Config.inlining_score_threshold,
      "Adjust threshold for expression score in order to apply inlining. \
       Default 10" );
    ("-tmm", Arg.Unit (add_opt "-tmm"), " Enable tail recursion modulo monoid");
    ("-tco", Arg.Unit (add_opt "-tco"), " Enable tail call optimisation");
    ("-tmc", Arg.Unit (add_opt "-tmc"), " Enable tail recursion modulo cons");
    ( "-dyn-lambdas",
      Arg.Set Common.Config.use_dynamic_lambdas,
      " Compile lambas using invokedynamic" );
  ]
  |> Arg.align

let () =
  Arg.parse speclist
    (fun _ ->
      print_endline usage_msg;
      exit 0)
    usage_msg;
  List.iter set_opt !opt_list;
  (match !program_input with
  | Some (File filename) -> Jvml.Run_jvml.compile_program_from_file filename
  | Some (String program_text) ->
      Jvml.Run_jvml.compile_program_from_string program_text
  | None ->
      prerr_endline usage_msg;
      exit 0)
  |> print_endline
