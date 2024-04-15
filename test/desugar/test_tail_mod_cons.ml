let run s =
  Parsing.Driver.parse_string s
  |> Typing.Infer.type_program_exn_from_string "test_env"
  |> Desugar.desugar_program
  |> Middle_end.Direct_calls.transform_direct_call_program
  |> Middle_end.Trmc.transform_tmc_program
  |> List.iter Desugar.Desugared_ast.pp_decl

let%expect_test "test List.map" =
  let program =
    {|
  val rec map = fun f -> fun l ->
    match l with
      | [] -> []
      | x::xs -> f x :: map f xs
  |}
  in
  let _ = run program in
  [%expect
    {|
    └──ValRec map_$0_dps
       └──Fun dst : () -> int -> ('a -> 'b) -> 'a list -> unit
          └──Fun i : int -> ('a -> 'b) -> 'a list -> unit
             └──Fun f_$0 : ('a -> 'b) -> 'a list -> unit
                └──Fun l_$0 : 'a list -> unit
                   └──Let desugar_t0_$0
                      └──Ident l_$0 : 'a list
                      └──Switch
                         └──Ident desugar_t0_$0 : 'a list
                         └── <case>
                            └──Nil$ : tag=0
                            └──Shared
                               └──Set_Tuple
                                  └──Ident i : int
                                  └──Ident dst : ()
                                  └──Constructor Nil$ : 'b list
                         └── <case>
                            └──Cons$ : tag=1
                            └──Shared
                               └──Let x_$0
                                  └──Get 0
                                     └──GetArg
                                        └──Ident desugar_t0_$0 : 'a list
                                  └──Let xs_$0
                                     └──Get 1
                                        └──GetArg
                                           └──Ident desugar_t0_$0 : 'a list
                                     └──Let dst'
                                        └──Tuple : ('b * 'b list)
                                           └──App
                                              └──Ident f_$0 : 'a -> 'b
                                              └──Ident x_$0 : 'a
                                           └──Hole
                                        └──Seq
                                           └──Set_Tuple
                                              └──Ident i : int
                                              └──Ident dst : ('b * 'b list)
                                              └──App
                                                 └──Constructor Cons$ : ('b * 'b list) -> 'b list
                                                 └──Ident dst' : ('b * 'b list)
                                           └──Direct_app : map_$0_dps
                                              └──Ident dst' : ('b * 'b list)
                                              └──Int 1
                                              └──Ident f_$0 : 'a -> 'b
                                              └──Ident xs_$0 : 'a list
    └──ValRec map_$0
       └──Fun f_$0 : ('a -> 'b) -> 'a list -> 'b list
          └──Fun l_$0 : 'a list -> 'b list
             └──Let desugar_t0_$0
                └──Ident l_$0 : 'a list
                └──Switch
                   └──Ident desugar_t0_$0 : 'a list
                   └── <case>
                      └──Nil$ : tag=0
                      └──Shared
                         └──Constructor Nil$ : 'b list
                   └── <case>
                      └──Cons$ : tag=1
                      └──Shared
                         └──Let x_$0
                            └──Get 0
                               └──GetArg
                                  └──Ident desugar_t0_$0 : 'a list
                            └──Let xs_$0
                               └──Get 1
                                  └──GetArg
                                     └──Ident desugar_t0_$0 : 'a list
                               └──Let dst
                                  └──Tuple : ('b * 'b list)
                                     └──App
                                        └──Ident f_$0 : 'a -> 'b
                                        └──Ident x_$0 : 'a
                                     └──Hole
                                  └──Seq
                                     └──Direct_app : map_$0_dps
                                        └──Ident dst : ('b * 'b list)
                                        └──Int 1
                                        └──Ident f_$0 : 'a -> 'b
                                        └──Ident xs_$0 : 'a list
                                     └──App
                                        └──Constructor Cons$ : ('b * 'b list) -> 'b list
                                        └──Ident dst : ('b * 'b list)

  |}]

let%expect_test "test multiple possible tail rec mod cons" =
  let program =
    {|
  type expr = Int of int | If of expr * expr * expr

  val rec map_tail = fun f -> fun e ->
    match e with
      | Int i -> f e
      | If (e0, e1, e2) -> If(e0, map_tail f e1, map_tail f e2)
  |}
  in
  let _ = run program in
  [%expect
    {|
    └──Type expr
       └──params = []
       └──constructors
          └──Int of int : tag=0
          └──If of (expr * expr * expr) : tag=1
    └──ValRec map_tail_$0_dps
       └──Fun dst : () -> int -> (expr -> expr) -> expr -> unit
          └──Fun i : int -> (expr -> expr) -> expr -> unit
             └──Fun f_$0 : (expr -> expr) -> expr -> unit
                └──Fun e_$0 : expr -> unit
                   └──Let desugar_t1_$0
                      └──Ident e_$0 : expr
                      └──Switch
                         └──Ident desugar_t1_$0 : expr
                         └── <case>
                            └──Int : tag=0
                            └──Shared
                               └──Let i_$0
                                  └──GetArg
                                     └──Ident desugar_t1_$0 : expr
                                  └──Set_Tuple
                                     └──Ident i : int
                                     └──Ident dst : ()
                                     └──App
                                        └──Ident f_$0 : expr -> expr
                                        └──Ident e_$0 : expr
                         └── <case>
                            └──If : tag=1
                            └──Shared
                               └──Let e0_$0
                                  └──Get 0
                                     └──GetArg
                                        └──Ident desugar_t1_$0 : expr
                                  └──Let e1_$0
                                     └──Get 1
                                        └──GetArg
                                           └──Ident desugar_t1_$0 : expr
                                     └──Let e2_$0
                                        └──Get 2
                                           └──GetArg
                                              └──Ident desugar_t1_$0 : expr
                                        └──Let dst'
                                           └──Tuple : (expr * expr * expr)
                                              └──Ident e0_$0 : expr
                                              └──Hole
                                              └──Direct_app : map_tail_$0
                                                 └──Ident f_$0 : expr -> expr
                                                 └──Ident e2_$0 : expr
                                           └──Seq
                                              └──Set_Tuple
                                                 └──Ident i : int
                                                 └──Ident dst : (expr * expr * expr)
                                                 └──App
                                                    └──Constructor If : (expr * expr * expr) -> expr
                                                    └──Ident dst' : (expr * expr * expr)
                                              └──Direct_app : map_tail_$0_dps
                                                 └──Ident dst' : (expr * expr * expr)
                                                 └──Int 1
                                                 └──Ident f_$0 : expr -> expr
                                                 └──Ident e1_$0 : expr
    └──ValRec map_tail_$0
       └──Fun f_$0 : (expr -> expr) -> expr -> expr
          └──Fun e_$0 : expr -> expr
             └──Let desugar_t1_$0
                └──Ident e_$0 : expr
                └──Switch
                   └──Ident desugar_t1_$0 : expr
                   └── <case>
                      └──Int : tag=0
                      └──Shared
                         └──Let i_$0
                            └──GetArg
                               └──Ident desugar_t1_$0 : expr
                            └──App
                               └──Ident f_$0 : expr -> expr
                               └──Ident e_$0 : expr
                   └── <case>
                      └──If : tag=1
                      └──Shared
                         └──Let e0_$0
                            └──Get 0
                               └──GetArg
                                  └──Ident desugar_t1_$0 : expr
                            └──Let e1_$0
                               └──Get 1
                                  └──GetArg
                                     └──Ident desugar_t1_$0 : expr
                               └──Let e2_$0
                                  └──Get 2
                                     └──GetArg
                                        └──Ident desugar_t1_$0 : expr
                                  └──Let dst
                                     └──Tuple : (expr * expr * expr)
                                        └──Ident e0_$0 : expr
                                        └──Hole
                                        └──Direct_app : map_tail_$0
                                           └──Ident f_$0 : expr -> expr
                                           └──Ident e2_$0 : expr
                                     └──Seq
                                        └──Direct_app : map_tail_$0_dps
                                           └──Ident dst : (expr * expr * expr)
                                           └──Int 1
                                           └──Ident f_$0 : expr -> expr
                                           └──Ident e1_$0 : expr
                                        └──App
                                           └──Constructor If : (expr * expr * expr) -> expr
                                           └──Ident dst : (expr * expr * expr)

  |}]

let%expect_test "test " =
  let program =
    {|
type 'a fancy_list = N | C of 'a * 'a fancy_list | Z of 'a fancy_list * 'a

val rec fancy_map = fun f -> fun l ->
  match l with
    | N -> N
    | C(x, xs) -> C(f x, fancy_map f xs)
    | Z(xs, x) -> Z(fancy_map f xs, f x)
|}
  in
  let _ = run program in
  [%expect
    {|
  └──Type fancy_list
     └──params = ['a]
     └──constructors
        └──N : tag=0
        └──C of ('a * 'a fancy_list) : tag=1
        └──Z of ('a fancy_list * 'a) : tag=2
  └──ValRec fancy_map_$0_dps
     └──Fun dst : () -> int -> ('a -> 'b) -> 'a fancy_list -> unit
        └──Fun i : int -> ('a -> 'b) -> 'a fancy_list -> unit
           └──Fun f_$0 : ('a -> 'b) -> 'a fancy_list -> unit
              └──Fun l_$0 : 'a fancy_list -> unit
                 └──Let desugar_t2_$0
                    └──Ident l_$0 : 'a fancy_list
                    └──Switch
                       └──Ident desugar_t2_$0 : 'a fancy_list
                       └── <case>
                          └──N : tag=0
                          └──Shared
                             └──Set_Tuple
                                └──Ident i : int
                                └──Ident dst : ()
                                └──Constructor N : 'b fancy_list
                       └── <case>
                          └──C : tag=1
                          └──Shared
                             └──Let x_$0
                                └──Get 0
                                   └──GetArg
                                      └──Ident desugar_t2_$0 : 'a fancy_list
                                └──Let xs_$0
                                   └──Get 1
                                      └──GetArg
                                         └──Ident desugar_t2_$0 : 'a fancy_list
                                   └──Let dst'
                                      └──Tuple : ('b * 'b fancy_list)
                                         └──App
                                            └──Ident f_$0 : 'a -> 'b
                                            └──Ident x_$0 : 'a
                                         └──Hole
                                      └──Seq
                                         └──Set_Tuple
                                            └──Ident i : int
                                            └──Ident dst : ('b * 'b fancy_list)
                                            └──App
                                               └──Constructor C : ('b * 'b fancy_list) -> 'b fancy_list
                                               └──Ident dst' : ('b * 'b fancy_list)
                                         └──Direct_app : fancy_map_$0_dps
                                            └──Ident dst' : ('b * 'b fancy_list)
                                            └──Int 1
                                            └──Ident f_$0 : 'a -> 'b
                                            └──Ident xs_$0 : 'a fancy_list
                       └── <case>
                          └──Z : tag=2
                          └──Shared
                             └──Let xs_$1
                                └──Get 0
                                   └──GetArg
                                      └──Ident desugar_t2_$0 : 'a fancy_list
                                └──Let x_$1
                                   └──Get 1
                                      └──GetArg
                                         └──Ident desugar_t2_$0 : 'a fancy_list
                                   └──Let dst'
                                      └──Tuple : ('b fancy_list * 'b)
                                         └──Hole
                                         └──App
                                            └──Ident f_$0 : 'a -> 'b
                                            └──Ident x_$1 : 'a
                                      └──Seq
                                         └──Set_Tuple
                                            └──Ident i : int
                                            └──Ident dst : ('b fancy_list * 'b)
                                            └──App
                                               └──Constructor Z : ('b fancy_list * 'b) -> 'b fancy_list
                                               └──Ident dst' : ('b fancy_list * 'b)
                                         └──Direct_app : fancy_map_$0_dps
                                            └──Ident dst' : ('b fancy_list * 'b)
                                            └──Int 0
                                            └──Ident f_$0 : 'a -> 'b
                                            └──Ident xs_$1 : 'a fancy_list
  └──ValRec fancy_map_$0
     └──Fun f_$0 : ('a -> 'b) -> 'a fancy_list -> 'b fancy_list
        └──Fun l_$0 : 'a fancy_list -> 'b fancy_list
           └──Let desugar_t2_$0
              └──Ident l_$0 : 'a fancy_list
              └──Switch
                 └──Ident desugar_t2_$0 : 'a fancy_list
                 └── <case>
                    └──N : tag=0
                    └──Shared
                       └──Constructor N : 'b fancy_list
                 └── <case>
                    └──C : tag=1
                    └──Shared
                       └──Let x_$0
                          └──Get 0
                             └──GetArg
                                └──Ident desugar_t2_$0 : 'a fancy_list
                          └──Let xs_$0
                             └──Get 1
                                └──GetArg
                                   └──Ident desugar_t2_$0 : 'a fancy_list
                             └──Let dst
                                └──Tuple : ('b * 'b fancy_list)
                                   └──App
                                      └──Ident f_$0 : 'a -> 'b
                                      └──Ident x_$0 : 'a
                                   └──Hole
                                └──Seq
                                   └──Direct_app : fancy_map_$0_dps
                                      └──Ident dst : ('b * 'b fancy_list)
                                      └──Int 1
                                      └──Ident f_$0 : 'a -> 'b
                                      └──Ident xs_$0 : 'a fancy_list
                                   └──App
                                      └──Constructor C : ('b * 'b fancy_list) -> 'b fancy_list
                                      └──Ident dst : ('b * 'b fancy_list)
                 └── <case>
                    └──Z : tag=2
                    └──Shared
                       └──Let xs_$1
                          └──Get 0
                             └──GetArg
                                └──Ident desugar_t2_$0 : 'a fancy_list
                          └──Let x_$1
                             └──Get 1
                                └──GetArg
                                   └──Ident desugar_t2_$0 : 'a fancy_list
                             └──Let dst
                                └──Tuple : ('b fancy_list * 'b)
                                   └──Hole
                                   └──App
                                      └──Ident f_$0 : 'a -> 'b
                                      └──Ident x_$1 : 'a
                                └──Seq
                                   └──Direct_app : fancy_map_$0_dps
                                      └──Ident dst : ('b fancy_list * 'b)
                                      └──Int 0
                                      └──Ident f_$0 : 'a -> 'b
                                      └──Ident xs_$1 : 'a fancy_list
                                   └──App
                                      └──Constructor Z : ('b fancy_list * 'b) -> 'b fancy_list
                                      └──Ident dst : ('b fancy_list * 'b)

|}]
