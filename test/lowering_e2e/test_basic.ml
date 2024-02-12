let build_and_run prog =
  let _ = if not (Sys.file_exists "tmp/") then Sys.mkdir "tmp/" 0o755 in
  let tmp = open_out "tmp/test.j" in
  let bytecode = Jvml.Run_jvml.compile_program_from_string prog in
  Out_channel.output_string tmp bytecode;
  Out_channel.close tmp;
  Sys.command "bash build_run_e2e.sh"

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

let%expect_test "test boolean OR" =
  let program =
    {|
  val or0 = print(false || false)
  val or1 = print(false || true)
  val or2 = print(true || false)
  val or3 = print(true || true)
  |}
  in
  let _ = build_and_run program in
  [%expect {|
  false
  true
  true
  true
  |}]

let%expect_test "test boolean AND" =
  let program =
    {|
  val and0 = print(false && false)
  val and1 = print(false && true)
  val and2 = print(true && false)
  val and3 = print(true && true)
  |}
  in
  let _ = build_and_run program in
  [%expect {|
  false
  false
  false
  true
  |}]

let%expect_test "test AND short-circuiting" =
  let program = {|val x = false && ((fun x -> true) (print 3)) |} in
  let _ = build_and_run program in
  [%expect {||}]

let%expect_test "test OR short-circuiting" =
  let program = {|val x = true || ((fun x -> true) (print 3))|} in
  let _ = build_and_run program in
  [%expect {||}]

let%expect_test "test compiling simple type union" =
  let program =
    {|
  type animal = Cat | Dog
  val x = print(Cat)
  val y = print(Dog)
  |}
  in
  let _ = build_and_run program in
  [%expect {|
  Cat
  Dog
  |}]

let%expect_test "test compiling simple type product" =
  let program =
    {|
  type assoc = Entry of (int * bool)
  val x = print (Entry (1, true))
  |}
  in
  let _ = build_and_run program in
  [%expect {|
  Entry ([1, true])
  |}]

let%expect_test "test recursive type product" =
  let program =
    {|
    type num = Z | S of num
    val x = print(Z)
    val y = print(S (S (S Z)))
  |}
  in
  let _ = build_and_run program in
  [%expect {|
  Z
  S (S (S (Z)))
  |}]

let%expect_test "test equality of adts" =
  let program =
    {|
  type 'a list = N | C of int * 'a list
  val compnil = print(N = N)
  val compdiff = print(N = C(0, N))
  val comp_eqcons = print(C(0, N) = C(0, N))
  val comp_almost_eq_cons = print(C(0, N) = C(1, N))

  val deepnesting_pos = print(C(0, C(1, C(2, N))) = C(0, C(1, C(2, N))))
  val deepnesting_false = print(C(0, C(1, C(2, N))) = C(0, C(1, C(2, C(3, N)))))
    |}
  in
  let _ = build_and_run program in
  [%expect {|
  true
  false
  true
  false
  true
  false
  |}]

let%expect_test "test tuple equality" =
  let program =
    {|
    val a = (1, 2)
    val b = (1, 2)
    val c = (1, 3)
    val d = (3, 2)

    val f = print(a = b)
    val g = print(a = c)
    val h = print(a = d)
    val i = print(c = d)
    |}
  in
  let _ = build_and_run program in
  [%expect {|
    true
    false
    false
    false
    |}]

let%expect_test "test basic sequence" =
  let program = {|
  val a = do {(); print(true); 3}
  val b = print(a)
  |} in
  let _ = build_and_run program in
  [%expect {|
  true
  3
  |}]

let%expect_test "test recursive function" =
  let factorial =
    {|
  val rec fact = fun n -> if n = 0 then 1 else n * fact (n - 1)
  val test = do {print(fact 0); print (fact 1); print (fact 2); print (fact 5); print(fact 6)}
  |}
  in
  let _ = build_and_run factorial in
  [%expect {|
  1
  1
  2
  120
  720
  |}]

let%expect_test "test let rec" =
  let factorial_letrec =
    {|
  val fact = let rec fact_inner = fun n -> if n = 0 then 1 else n * fact_inner (n - 1) in fact_inner
  val test = do {print(fact 0); print (fact 1); print (fact 2); print (fact 5); print(fact 6)}
  |}
  in
  let _ = build_and_run factorial_letrec in
  [%expect {|
  1
  1
  2
  120
  720
  |}]

let%expect_test "test basic int match" =
  let program =
    {|
  val sub = fun x -> match x with 0 -> 0 | n -> (n - 1)
  val test = do {
    print(sub 0);
    print(sub 1);
    print(sub 2)
  }
  |}
  in
  let _ = build_and_run program in
  [%expect {|
    0
    0
    1 |}]

let%expect_test "test basic boolean match" =
  let program =
    {|
  val flip = fun x -> match x with true -> false | false -> true
  val test = do {
    print(flip true);
    print(flip false)
  }
  |}
  in
  let _ = build_and_run program in
  [%expect {|
    false
    true |}]

let%expect_test "test basic unit match" =
  let program =
    {|
  val matchunit = fun x -> match x with () -> ()
  val test = print(matchunit ())
  |}
  in
  let _ = build_and_run program in
  [%expect {|()|}]

let%expect_test "test basic no arg ADT (tag) match" =
  let program =
    {|
  type either = X | Y
  val to_int = fun x -> match x with X -> 10 | Y -> 20
  val test = do {
    print(to_int X);
    print(to_int Y)
  }
  |}
  in
  let _ = build_and_run program in
  [%expect {|
    10
    20|}]

let%expect_test "test basic ADT match with args" =
  let program =
    {|
    type either = X of int | Y
    val to_int = fun x -> match x with X x -> x | Y -> 20
    val test = do {
      print(to_int (X 1));
      print(to_int (X 2));
      print(to_int Y)
    }
    |}
  in
  let _ = build_and_run program in
  [%expect {|
      1
      2
      20|}]

let%expect_test "test deeply nested ADT match" =
  let program =
    {|
    type nat = Z | S of nat
    val to_int = fun x -> match x with
      | Z -> 10
      | S Z -> 20
      | S S Z -> 30
      | S S S Z -> 40
      | x -> 50

    val test = do {
      print(to_int(S(S (S (S (Z))))));
      print(to_int(S (S (S (Z)))));
      print(to_int(S (S (Z))));
      print(to_int(S (Z)));
      print(to_int(Z))
    }

    |}
  in
  let _ = build_and_run program in
  [%expect {|
      50
      40
      30
      20
      10|}]
