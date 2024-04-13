(* standard binary op - not a short circuiting operator *)
type standard_bop =
  | ADD
  | SUB
  | MUL
  | DIV
  | EQ
  | LT
  | GT
  | FLOAT_ADD
  | FLOAT_SUB
  | FLOAT_MUL
  | FLOAT_DIV
  | STRING_CONCAT
[@@deriving show]

type type_expr =
  | TyInt
  | TyFloat
  | TyString
  | TyBool
  | TyUnit
  | TyCustom of string
  | TyAny (* erased tyvar *)
  | TyTuple of type_expr list
  | TyFun of type_expr * type_expr
  | TyArray of type_expr
[@@deriving show]

type switch_type = LOOKUP | TABLE of int [@@deriving show]

type instruction =
  | PUSH_INT of Int32.t
  | BOX_INT
  | UNBOX_INT
  | PUSH_FLOAT of float
  | BOX_FLOAT
  | UNBOX_FLOAT
  | PUSH_BOOL of bool
  | BOX_BOOL
  | UNBOX_BOOL
  | PUSH_UNIT
  | PUSH_STRING of string
  | STORE_REF of int
  | LOAD_REF of int
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
  | POP
  (* save return type in order to cast *)
  | APPLY of type_expr
  (* method(name, args, return type), actual return type (unerased) *)
  | STATIC_APPLY of string * type_expr list * type_expr * type_expr
  (* class, field, type *)
  | LOAD_STATIC of string * string * type_expr
  | STORE_STATIC of string * string * type_expr
  (* freevar types, arg, return *)
  | CREATE_DYNAMIC_CLOSURE of string * type_expr list * type_expr * type_expr
  | TUPLE_GET of type_expr * Int32.t
  | CONSTRUCTOR_GET of type_expr * string
  (* (int_to_test, label), default label *)
  | SWITCH of switch_type * (Int32.t * string) list * string
  | MATCH_FAILURE
  | CONSTRUCTOR_INDEX of string
  | NULL
  | LOAD_FIELD_ANY_CLASS of string * string * type_expr
  | STORE_FIELD_ANY_CLASS of string * string * type_expr
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

type constructor = {
  name : string;
  tag : Int32.t;
  tname : string;
  arg : type_expr option;
}
[@@deriving show]

type declaration =
  | Closure of closure
  | Type_interface of type_interface
  | Constructor of constructor
[@@deriving show]

type static_method = {
  name : string;
  args : type_expr list;
  return_type : type_expr;
  body : instruction list;
}
[@@deriving show]

type program = {
  declarations : declaration list;
  code : instruction list;
  static_methods : static_method list;
}
[@@deriving show]
