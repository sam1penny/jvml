type oper = ADD | SUB | MUL | DIV | EQ | LT | GT | AND | OR

type const =
  | Int of int
  | Ident of string
  | Constr of string
  | Bool of bool
  | Unit
and expr =
  | Expr_Const of const
  | Oper of expr * oper * expr
  | If of expr * expr * expr
  | Fun of string * expr
  | App of expr * expr
  | Match of expr * case list
  | Tuple of expr list
  | Let of string * expr * expr

and pattern =
  | Pat_Const of const
  | Pat_Any
  | Pat_Or of pattern * pattern
  | Pat_Tuple of pattern list
  | Pat_Constr of string * pattern
and case =
  | Case of pattern * expr
and type_expr =
  | TINT
  | TBOOL
  | TUNIT
  | TCUSTOM of string
  | TPARAM of string
  | TTUPLE of type_expr list
  | TFUN of type_expr * type_expr
and type_constr =
  | DeclConstr of string * type_expr option
and decl =
  | Val of string * expr
  | Type of string list * string * type_constr list

let string_of_const = function
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Ident ident -> ident
  | Constr c -> c
  | Unit -> "()"

let string_of_oper = function
  | ADD -> "+"
  | MUL  -> "*"
  | DIV  -> "/"
  | SUB -> "-"
  | LT   -> "<"
  | GT -> ">"
  | EQ -> "="
  | AND   -> "&&"
  | OR   -> "||"


let rec pp_pattern = function
  | Pat_Const c -> string_of_const c
  | Pat_Any -> "_"
  | Pat_Or (p1, p2) -> Printf.sprintf "%s | %s" (pp_pattern p1) (pp_pattern p2)
  | Pat_Tuple pats -> Printf.sprintf "(%s)" (List.map pp_pattern pats |> String.concat ",")
  | Pat_Constr (cname, p) -> Printf.sprintf "%s %s" cname (pp_pattern p)
let rec pp_case = function
  | Case(p, e) -> Printf.sprintf "[%s -> %s]" (pp_pattern p) (pp_expr e)
and
pp_expr = function
  | Expr_Const c -> string_of_const c
  | Oper (e1, op, e2) -> Printf.sprintf "{%s %s %s}" (pp_expr e1) (string_of_oper op) (pp_expr e2)
  | If (e1, e2, e3) -> Printf.sprintf "[if %s then %s else %s]" (pp_expr e1) (pp_expr e2) (pp_expr e3)
  | Fun (i, e) -> Printf.sprintf "[fun %s -> %s]" i (pp_expr e)
  | App(e1, e2) -> Printf.sprintf "(%s) %s" (pp_expr e1) (pp_expr e2)
  | Match(e, cl) -> Printf.sprintf "[match %s with %s]" (pp_expr e) (List.map pp_case cl |> String.concat " | ")
  | Tuple es -> Printf.sprintf "(%s)" (List.map pp_expr es |> String.concat ",")
  | Let(x, e1, e2) -> Printf.sprintf "let %s = %s in %s" x (pp_expr e1) (pp_expr e2)
  (* | _ -> raise @@ Invalid_argument "printing not supported for this constructor!" *)

let rec pp_texpr = function
  | TINT -> "int"
  | TBOOL -> "bool"
  | TUNIT -> "unit"
  | TCUSTOM s -> s
  | TPARAM s -> "\'" ^ s
  | TTUPLE ts -> List.map pp_texpr ts |> String.concat "*" |> Printf.sprintf "(%s)"
  | TFUN (t1, t2) -> Printf.sprintf "%s -> %s" (pp_texpr t1) (pp_texpr t2)

let pp_tconstr = function
  | DeclConstr(s, None) -> s
  | DeclConstr(s, Some texpr) -> Printf.sprintf "%s of %s" s (pp_texpr texpr)

let pp_decl = function
  | Val(v, e) -> Printf.sprintf "%s = %s" v (pp_expr e)
  | Type(params, t, tconstrs) -> Printf.sprintf "type %s %s = %s"
    (List.map (fun p -> "\'" ^ p) params |> String.concat "," |> Printf.sprintf "(%s)")
    t (List.map pp_tconstr tconstrs |> String.concat " | ")