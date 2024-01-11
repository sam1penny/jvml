type instruction =
  | PUSH_INT of int
  | BOX_INT
  | UNBOX_INT
  | PUSH_UNIT
  | STORE_REF of string
  | LOAD_REF of string
  | IFZERO of string
  | GOTO of string
  | LABEL of string
  | BOP of Common.bop
  | LOAD_FIELD of string * Typing.Typed_ast.type_expr
  | STORE_FIELD of string * Typing.Typed_ast.type_expr
  | ALLOC_CLOSURE of string
  (* closure label, arg_types *)
  | CONSTRUCT_CLOSURE of string * Typing.Typed_ast.type_expr list
  (* save return type in order to cast *)
  | APPLY of Typing.Typed_ast.type_expr
  (* class, field, type *)
  | LOAD_STATIC of string * string * Typing.Typed_ast.type_expr
  | STORE_STATIC of string * string * Typing.Typed_ast.type_expr
[@@deriving show]

type closure =
  (*
  name, (fvar name, fvar type) list
  arg type, return type, body
  *)
  | CLOSURE of
      string
      * (string * Typing.Typed_ast.type_expr) list
      * Typing.Typed_ast.type_expr
      * Typing.Typed_ast.type_expr
      * instruction list
[@@deriving show]

let show_program p = List.map show_instruction p |> String.concat "\n"
