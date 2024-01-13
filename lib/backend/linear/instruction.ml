(* standard binary op - not a short circuiting operator *)
type standard_bop = ADD | SUB | MUL | DIV | EQ | LT | GT [@@deriving show]

type instruction =
  | PUSH_INT of int
  | BOX_INT
  | UNBOX_INT
  | PUSH_BOOL of bool
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
  | ALLOC_OBJ of string
  (* closure label, arg_types *)
  | CONSTRUCT_OBJ of string * Typing.Typed_ast.type_expr list
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

type type_interface = { name : string; constructors : string list }
[@@deriving show]

type constructor = {
  name : string;
  tname : string;
  arg : Typing.Typed_ast.type_expr option;
}
[@@deriving show]

type declaration =
  | Closure of closure
  | Type_interface of type_interface
  | Constructor of constructor
[@@deriving show]

let show_program p = List.map show_instruction p |> String.concat "\n"
