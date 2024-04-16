Common.Config.set_all_opt ()

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
  type 'a my_list = N | C of int * 'a my_list
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
    type either = A of int | B
    val to_int = fun x -> match x with A x -> x | B -> 20
    val test = do {
      print(to_int (A 1));
      print(to_int (A 2));
      print(to_int B)
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
      | S (S Z) -> 30
      | S (S (S Z)) -> 40
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

let%expect_test "test multiple nested lambdas for lifting" =
  let program =
    {|
    val run_quad =
      let double = fun x -> x * 2 in
      let quad = fun y -> double y * double y in
      quad 4
    val test = print(run_quad)
    |}
  in
  let _ = build_and_run program in
  [%expect {|
  64
  |}]

let%expect_test "test lambda lift mutual recursion" =
  let program =
    {|
    val rec iseven = fun x ->
      let rec isodd = fun y -> if y = 0 then false else iseven (y - 1) in
      if x = 0 then true else isodd (x - 1)
    val test = do {
      print(iseven 10);
      print(iseven 11);
      print(iseven 12)
    }
    |}
  in
  let _ = build_and_run program in
  [%expect {|
    true
    false
    true
  |}]

let%expect_test "test one-arg tail call" =
  let program =
    {|
  val rec count = fun n -> if n = 0 then 1 else count (n - 1)
  val test = print(count 20000)
  |}
  in
  let _ = build_and_run program in
  [%expect {|1|}]

let%expect_test "test multiple arg tail call" =
  let program =
    {|
  val rec count2 = fun x -> fun y ->
    if x = 0 then y
    else count2 (x - 1) y

  val test = print(count2 20000 6)
  |}
  in
  let _ = build_and_run program in
  [%expect {|6|}]

let%expect_test "test tail call with match" =
  let program =
    {|
  val rec count = fun x ->
    match x with
      | 0 -> 1
      | _ -> count (x - 1)

  val test = print(count 20000)
  |}
  in
  let _ = build_and_run program in
  [%expect {|1|}]

let%expect_test "test tail call with ADT" =
  let program =
    {|
  type 'a my_list = N | C of 'a * 'a my_list
  val rec length_tr = fun l -> fun acc ->
    match l with
      | N -> acc
      | C(_, tl) -> length_tr tl (acc+1)
  val test = print(length_tr (C(1, C(2, C(3, N)))) 0)
  |}
  in
  let _ = build_and_run program in
  [%expect {|3|}]

(*
tests for tail recursion modulo monoid
*)

let%expect_test "test trmm - simple recursive sum" =
  let program =
    {|
  val rec sum = fun n ->
    if n = 0 then 0
    else n + sum (n - 1)

  val test = do {
    print(sum 2);
    print(sum 10)
  }
  |}
  in
  let _ = build_and_run program in
  [%expect {|
  3
  55
  |}]

let%expect_test "test trmm - with tail call" =
  let program =
    {|
  val rec foo = fun n ->
    if n = 0 then 0
    else if n = 1 then foo (n - 1)
    else n + foo (n - 1)

  val test = do {
    print(foo 2);
    print(foo 10)
  }
  |}
  in
  let _ = build_and_run program in
  [%expect {|
  2
  54
  |}]

let%expect_test "test trmm - test one call suitable for trmc, one not" =
  let program =
    {|
  val rec foo = fun n ->
    if n < 2 then 1
    else foo(n - 2) + foo (n - 1)

  val test = do {
    print(foo 2);
    print(foo 3);
    print(foo 4);
    print(foo 5);
    print(foo 6)
  }
  |}
  in
  let _ = build_and_run program in
  [%expect {|
    2
    3
    5
    8
    13
  |}]

let%expect_test "test floats" =
  let program =
    {|
  val test = do {
    print(1.0 +. 5.0);
    print(2.0 *. 10.0);
    print(4.0 /. 3.0);
    print(4.0 -. 3.0)
    }
  |}
  in
  let _ = build_and_run program in
  [%expect {|
    6.0
    20.0
    1.3333334
    1.0 |}]

let%expect_test "test strings" =
  let program =
    {|
  val x = print("abc")
  val y = print("ABC")
  val z = print("aBc523")
  val a = print("a" ^ "b")
  val b = print("a" ^ "b" ^ "c")

  val test =
    let poly = fun x -> x in
    let foo = "abc" in
    let concata = fun y -> "a" ^ y in
    do {
    print(poly foo);
    print(concata foo)
    }
  |}
  in
  let _ = build_and_run program in
  [%expect {|
  abc
  ABC
  aBc523
  ab
  abc
  abc
  aabc

|}]

(* testing for tail recursion modulo cons*)

let%expect_test "test tail rec mod cons List.map" =
  let program =
    {|
val rec map = fun f -> fun l ->
  match l with
    | [] -> []
    | x::xs -> f x :: map f xs

val test = print(map (fun x -> x + 1) [1;2;3])
|}
  in
  let _ = build_and_run program in
  [%expect {|[2;3;4]|}]

let%expect_test "test tail rec mod cons multiple choices one branch" =
  let program =
    {|
type expr = Int of int | If of expr * expr * expr

val rec map_tail = fun f -> fun e ->
  match e with
    | Int i -> f e
    | If (e0, e1, e2) -> If(e0, map_tail f e1, map_tail f e2)

val test_expr = If (Int 1, If(Int 2, Int 3, Int 4), If(Int 5, Int 6, Int 7))

val test =
  let replace_with_ten = fun x -> Int 10 in
  print(map_tail replace_with_ten test_expr)
|}
  in
  let _ = build_and_run program in
  [%expect
    {| If ([Int (1), If ([Int (2), Int (10), Int (10)]), If ([Int (5), Int (10), Int (10)])]) |}]

let%expect_test "test tail rec mod cons multiple choices multiple branches" =
  let program =
    {|
type 'a fancy_list = N | C of 'a * 'a fancy_list | Z of 'a fancy_list * 'a

val rec fancy_map = fun f -> fun l ->
  match l with
    | N -> N
    | C(x, xs) -> C(f x, fancy_map f xs)
    | Z(xs, x) -> Z(fancy_map f xs, f x)

val test =
  let fancy = C(1, Z(C(3, Z(N, 4)), 2)) in
  print(fancy_map (fun x -> x + 1) fancy)
|}
  in
  let _ = build_and_run program in
  [%expect {| C ([2, Z ([C ([4, Z ([N, 5])]), 3])]) |}]

(*
tests for <= and >=
*)

let%expect_test "test <= and >=" =
  let program =
    {|

  val test_leq = do {
    print(0 <= 1);
    print(1 <= 1);
    print(2 <= 1)
  }

  val test_geq = do {
    print(0 >= 1);
    print(1 >= 1);
    print(2 >= 1)
  }
  |}
  in
  let _ = build_and_run program in
  [%expect {|
    true
    true
    false
    false
    true
    true
  |}]

let%expect_test "test uops" =
  let program =
    {|
  val test = do {
    print(real 3 +. 4.0);
    print(- 4 + 1);
    print(-. 4.0 +. 1.0);
    print(not true);
    print(not false)
  }
  |}
  in
  let _ = build_and_run program in
  [%expect {|
    7.0
    -3
    -3.0
    false
    true
  |}]

let%expect_test "test float comparisons" =
  let program =
    {|
  val test = do {
    print(3.0 >. 4.0);
    print(4.0 >=. 4.0);
    print(3.0 <. 4.0);
    print(3.0 <=. 4.0)
  }
  |}
  in
  let _ = build_and_run program in
  [%expect {|
    false
    true
    true
    true |}]
