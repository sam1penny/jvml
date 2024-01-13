(* standard binary op - not a short circuiting operator *)
type standard_bop = ADD | SUB | MUL | DIV | EQ | LT | GT [@@deriving show]

type type_expr =
  | TyInt
  | TyBool
  | TyUnit
  | TyCustom of string
  | TyAny (* erased tyvar *)
  | TyTuple of type_expr list
  | TyFun of type_expr * type_expr
  | TyArray of type_expr
[@@deriving show]

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
  | LOAD_FIELD of string * type_expr
  | STORE_FIELD of string * type_expr
  | ALLOC_OBJ of string
  (* closure label, arg_types *)
  | CONSTRUCT_OBJ of string * type_expr list
  | ALLOC_ARRAY of string
  | STORE_ARRAY
  | DUP
  (* save return type in order to cast *)
  | APPLY of type_expr
  (* class, field, type *)
  | LOAD_STATIC of string * string * type_expr
  | STORE_STATIC of string * string * type_expr
[@@deriving show]

type closure = {
  name : string;
  constructor_args : (string * type_expr) list;
  arg_type : type_expr;
  return_type : type_expr;
  body : instruction list;
}
[@@deriving show]

type type_interface = { name : string; constructors : string list }
[@@deriving show]

type constructor = { name : string; tname : string; arg : type_expr option }
[@@deriving show]

type declaration =
  | Closure of closure
  | Type_interface of type_interface
  | Constructor of constructor
[@@deriving show]

let show_program p = List.map show_instruction p |> String.concat "\n"
