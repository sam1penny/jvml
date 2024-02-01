let () =
  Jvml.Run_jvml.linear_ir_from_string "type either = X | Y val s = X"
  |> print_endline
