open Printf

let build_and_run prog =
  Sys.command (sprintf "bash build_run_e2e.sh -s \"%s\"" prog)

let%expect_test "basic arithmetic e2e" =
  let program = "val x = print (3 + 2 * 5)" in
  let _ = build_and_run program in
  [%expect {|13|}]

let%expect_test "test closures" =
  let program =
    "val x = let apply = fun f -> fun x -> f x in print(apply (fun x -> 2 * x) \
     3)"
  in
  let _ = build_and_run program in
  [%expect {|6|}]
