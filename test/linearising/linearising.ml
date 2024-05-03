let parse_type_desugar_linear_print s =
  Parsing.Driver.parse_string s
  |> Typing.Infer.type_program_exn_from_string "test_env"
  |> Desugar.desugar_program |> Linearise.Driver.lower_program_to_linear_ir
  |> Linearise.Instruction.pretty_print_program

let%expect_test "test stack linear code for simple addition" =
  let prog = {|
  val x = 3 + 4 + 5
  |} in
  let _ = parse_type_desugar_linear_print prog in
  [%expect
    {|
    (Instruction.PUSH_INT 3l)
    Instruction.BOX_INT
    Instruction.UNBOX_INT
    (Instruction.PUSH_INT 4l)
    Instruction.BOX_INT
    Instruction.UNBOX_INT
    (Instruction.BOP Instruction.ADD)
    Instruction.BOX_INT
    Instruction.UNBOX_INT
    (Instruction.PUSH_INT 5l)
    Instruction.BOX_INT
    Instruction.UNBOX_INT
    (Instruction.BOP Instruction.ADD)
    Instruction.BOX_INT
    (Instruction.STORE_STATIC ("Foo", "x_$0", Instruction.TyInt)) |}]

let%expect_test "test if statement is linearised correctly" =
  let prog = {|
  val test =
    if true then 1 else 0
  |} in
  let _ = parse_type_desugar_linear_print prog in
  [%expect
    {|
    (Instruction.PUSH_BOOL true)
    Instruction.BOX_BOOL
    Instruction.UNBOX_BOOL
    (Instruction.IFZERO "L0")
    (Instruction.PUSH_INT 1l)
    Instruction.BOX_INT
    (Instruction.GOTO "L1")
    (Instruction.LABEL "L0")
    (Instruction.PUSH_INT 0l)
    Instruction.BOX_INT
    (Instruction.LABEL "L1")
    (Instruction.STORE_STATIC ("Foo", "test_$0", Instruction.TyInt)) |}]

let%expect_test "test match statement is correctly compiled" =
  let prog =
    {|
  val test =
    match 3 with
      | 0 -> 0
      | 1 -> 1
      | y -> y
  |}
  in
  let _ = parse_type_desugar_linear_print prog in
  [%expect
    {|
    (Instruction.PUSH_INT 3l)
    Instruction.BOX_INT
    (Instruction.STORE_REF 1)
    (Instruction.LOAD_REF 1)
    Instruction.UNBOX_INT
    (Instruction.SWITCH (Instruction.LOOKUP, [(0l, "L3"); (1l, "L4")], "L6"))
    (Instruction.LABEL "L3")
    (Instruction.LABEL "L1")
    (Instruction.PUSH_INT 0l)
    Instruction.BOX_INT
    (Instruction.GOTO "L0")
    (Instruction.LABEL "L4")
    (Instruction.LABEL "L2")
    (Instruction.PUSH_INT 1l)
    Instruction.BOX_INT
    (Instruction.GOTO "L0")
    (Instruction.LABEL "L6")
    (Instruction.LABEL "L5")
    (Instruction.LOAD_REF 1)
    (Instruction.STORE_REF 2)
    (Instruction.LOAD_REF 2)
    (Instruction.GOTO "L0")
    (Instruction.LABEL "L0")
    (Instruction.STORE_STATIC ("Foo", "test_$0", Instruction.TyInt)) |}]

let%expect_test "test closure generation" =
  let prog = {|
    val double x = x * 2
    |} in
  let _ = parse_type_desugar_linear_print prog in
  [%expect
    {|
    (Instruction.Closure
       { Instruction.name = "Lambda$0"; constructor_args = [];
         arg_type = Instruction.TyInt; return_type = Instruction.TyInt;
         body =
         [(Instruction.LOAD_REF 1);
           (Instruction.STATIC_APPLY ("double_$0", [Instruction.TyInt],
              Instruction.TyInt, Instruction.TyInt))
           ]
         })
    (Instruction.ALLOC_OBJ "Lambda$0")
    (Instruction.CONSTRUCT_OBJ ("Lambda$0", []))
    (Instruction.STORE_STATIC ("Foo", "double_$0",
       (Instruction.TyFun (Instruction.TyInt, Instruction.TyInt))))
    { Instruction.name = "double_$0"; args = [Instruction.TyInt];
      return_type = Instruction.TyInt;
      body =
      [(Instruction.LOAD_REF 0); Instruction.UNBOX_INT;
        (Instruction.PUSH_INT 2l); Instruction.BOX_INT; Instruction.UNBOX_INT;
        (Instruction.BOP Instruction.MUL); Instruction.BOX_INT]
      } |}]
