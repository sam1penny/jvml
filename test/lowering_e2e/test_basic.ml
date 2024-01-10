open Printf

let build_and_run prog =
  Sys.command (sprintf "bash build_run_e2e.sh -s \"%s\"" prog)

let%expect_test "basic arithmetic e2e" =
  let program = "val x = let x = 3 + 2 * 5 in x" in
  let _ = build_and_run program in
  [%expect {|13|}]
