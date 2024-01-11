(* standard binary op - not a short circuiting operator *)
type standard_bop = ADD | SUB | MUL | DIV | EQ | LT | GT [@@deriving show]

type instruction =
  | PUSH_INT of int
  | BOX_INT
  | UNBOX_INT
  | BOX_BOOL
  | UNBOX_BOOL
  | PUSH_UNIT
  | STORE_REF of string
  | LOAD_REF of string
  | IFZERO of string
  | IFNONZERO of string
  | GOTO of string
  | LABEL of string
  | BOP of standard_bop
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

type closure = {
  name : string;
  constructor_args : (string * Typing.Typed_ast.type_expr) list;
  arg_type : Typing.Typed_ast.type_expr;
  return_type : Typing.Typed_ast.type_expr;
  body : instruction list;
}
[@@deriving show]

let show_program p = List.map show_instruction p |> String.concat "\n"
