let usage_msg = "compile -s <program_string> -f <file> <options>"

type program_input = String of string | File of string

let program_input = ref None
let opt_list : string list ref = ref []
let add_opt opt () = opt_list := opt :: !opt_list
let output_file = ref ""

let set_opt opt =
  match opt with
  | "-opt-all" -> Common.Config.set_all_opt ()
  | "-peep" -> Common.Config.set_all_peephole_opts ()
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
    ( "-c",
      Arg.Set_string Common.Config.generated_class_name,
      " Set name of generated class" );
    ("-o", Arg.Set_string output_file, " Set output file");
    ("-opt-all", Arg.Unit (add_opt "-opt-all"), " Enable all optimisations");
    ("-peep", Arg.Unit (add_opt "-peep"), " Enable peephole optimisations");
    ( "-const-fp",
      Arg.Unit (add_opt "-const-fp"),
      " Enable constant folding and propagation" );
    ("-inline", Arg.Unit (add_opt "-inline"), " Enable inlining");
    ( "-inl-threshold",
      Arg.Set_int Common.Config.inlining_score_threshold,
      " Adjust threshold for expression score in order to apply inlining. \
       Default 10" );
    ("-tmm", Arg.Unit (add_opt "-tmm"), " Enable tail recursion modulo monoid");
    ("-tco", Arg.Unit (add_opt "-tco"), " Enable tail call optimisation");
    ("-tmc", Arg.Unit (add_opt "-tmc"), " Enable tail recursion modulo cons");
    ( "-dyn-lambdas",
      Arg.Set Common.Config.use_dynamic_lambdas,
      " Compile lambas using invokedynamic" );
    ( "-peep-box",
      Arg.Unit
        (fun () ->
          Common.Config.do_peephole := true;
          Common.Config.do_peep_box := true),
      " Enable boxing peephole optimisation" );
    ( "-peep-push-pop",
      Arg.Unit
        (fun () ->
          Common.Config.do_peephole := true;
          Common.Config.do_peep_push_pop := true),
      " Enable push-pop peephole optimisation" );
    ( "-peep-goto-label",
      Arg.Unit
        (fun () ->
          Common.Config.do_peephole := true;
          Common.Config.do_peep_goto_label := true),
      " Enable goto-label peephole optimisation" );
    ( "-peep-store-load",
      Arg.Unit
        (fun () ->
          Common.Config.do_peephole := true;
          Common.Config.do_peep_store_load := true),
      " Enable store-load peephole optimisation" );
    ( "-dump_debug",
      Arg.String (fun x -> Common.Config.debug_file := Some x),
      " Dump debug information (currently just compile times) to json file" );
  ]
  |> Arg.align

let () =
  Arg.parse speclist
    (fun _ ->
      prerr_endline usage_msg;
      exit 1)
    usage_msg;
  List.iter set_opt !opt_list;
  let input =
    match !program_input with
    | Some i -> i
    | None ->
        prerr_endline "Input file not specified";
        exit 1
  in
  let out_chan =
    if !output_file == "" then (
      prerr_endline "Output file not specified";
      exit 1)
    else open_out !output_file
  in
  let jvm_assembly =
    match input with
    | File filename -> Jvml.Run_jvml.compile_program_from_file filename
    | String program_text ->
        Jvml.Run_jvml.compile_program_from_string program_text
  in
  (match !Common.Config.debug_file with
  | None -> ()
  | Some debug_file ->
      let debug_chan = open_out debug_file in
      Yojson.Basic.pretty_to_channel debug_chan !Common.debug_json;
      close_out debug_chan);
  Printf.fprintf out_chan "%s" jvm_assembly;
  close_out out_chan
