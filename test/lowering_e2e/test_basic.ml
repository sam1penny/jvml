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
