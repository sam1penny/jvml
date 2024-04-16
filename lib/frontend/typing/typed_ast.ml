open Common

type loc = Lexing.position * Lexing.position

type type_expr =
  | TyInt
  | TyFloat
  | TyString
  | TyBool
  | TyUnit
  | TyCustom of type_expr list * string
  | TyVar of string
  | TyTuple of type_expr list
  | TyFun of type_expr * type_expr
[@@deriving show]

type expr =
  | Int of loc * Int32.t
  | Float of loc * float
  | String of loc * string
  | Ident of loc * type_expr * string
  | Bool of loc * bool
  | Unit of loc
  | Bop of loc * type_expr * expr * Common.bop * expr
  | Uop of loc * type_expr * Common.uop * expr
  | If of loc * type_expr * expr * expr * expr
  | Fun of loc * type_expr * type_expr * string * expr
  | App of loc * type_expr * expr * expr
  | Match of loc * type_expr * expr * (pattern * expr) list
  | Tuple of loc * type_expr * expr list
  | Let of loc * type_expr * string * expr * expr
  | LetRec of loc * type_expr * string * expr * expr
  | Constr of loc * type_expr * string
  | Seq of loc * type_expr * expr list

and pattern =
  | Pat_Int of loc * Int32.t
  | Pat_Ident of loc * type_expr * string
  | Pat_Bool of loc * bool
  | Pat_Unit of loc
  | Pat_Any of loc * type_expr
  | Pat_Or of loc * type_expr * pattern list
  | Pat_Tuple of loc * type_expr * pattern list
  | Pat_Constr of loc * type_expr * string * pattern option

type type_constr = DeclConstr of loc * string * type_expr option

type decl =
  | Val of loc * type_expr * string * expr
  | ValRec of loc * type_expr * string * expr
  | Type of loc * type_expr * string list * string * type_constr list

(** important - call only *once* for a particularly operator. EQ is polymorphic *)
let bop_arg_type nt ty =
  let open Common in
  match ty with
  | ADD | SUB | MUL | DIV | LT | GT | LEQ | GEQ -> TyInt
  | AND | OR -> TyBool
  | FLOAT_ADD | FLOAT_SUB | FLOAT_MUL | FLOAT_DIV -> TyFloat
  | STRING_CONCAT -> TyString
  | EQ -> nt ()

let bop_return_type ty =
  let open Common in
  match ty with
  | ADD | SUB | MUL | DIV -> TyInt
  | AND | OR | LT | GT | LEQ | GEQ | EQ -> TyBool
  | FLOAT_ADD | FLOAT_SUB | FLOAT_MUL | FLOAT_DIV -> TyFloat
  | STRING_CONCAT -> TyString

let uop_arg_type ty =
  let open Common in
  match ty with NEG -> TyInt | REAL -> TyInt | FLOAT_NEG -> TyFloat

let uop_return_type ty =
  let open Common in
  match ty with NEG -> TyInt | REAL -> TyFloat | FLOAT_NEG -> TyFloat

(* printing *)
let rec pp_texpr = function
  | TyInt -> "int"
  | TyFloat -> "float"
  | TyString -> "string"
  | TyBool -> "bool"
  | TyUnit -> "unit"
  | TyVar v -> v
  | TyCustom ([], v) -> v
  | TyCustom ([ t ], v) -> Printf.sprintf "%s %s" (pp_texpr t) v
  | TyCustom (t :: ts, v) ->
      Printf.sprintf "(%s) %s"
        (List.map pp_texpr (t :: ts) |> String.concat ", ")
        v
  | TyTuple ts ->
      List.map
        (fun t ->
          match t with TyFun _ -> "(" ^ pp_texpr t ^ ")" | _ -> pp_texpr t)
        ts
      |> String.concat " * " |> Printf.sprintf "(%s)"
  | TyFun (f, c) -> (
      (match f with TyFun _ -> "(" ^ pp_texpr f ^ ")" | _ -> pp_texpr f)
      ^ " -> "
      ^ match c with TyFun _ -> "" ^ pp_texpr c ^ "" | _ -> pp_texpr c)
[@@deriving show]

let string_of_expr_node =
  let open Printf in
  function
  | Int (_, i) -> sprintf "Int %ld" i
  | Float (_, f) -> sprintf "Float %f" f
  | String (_, s) -> sprintf "String %s" s
  | Bool (_, b) -> sprintf "Bool %b" b
  | Ident (_, ty, ident) -> sprintf "Ident %s : %s" ident (pp_texpr ty)
  | Unit _ -> "()"
  | Bop (_, return_ty, _, op, _) ->
      sprintf "Bop %s : %s" (show_bop op) (pp_texpr return_ty)
  | Uop (_, ty, op, _) -> sprintf "Uop %s : %s" (show_uop op) (pp_texpr ty)
  | If _ -> "If"
  | Fun (_, arg_type, return_type, x, _) ->
      sprintf "Fun %s : %s" x (pp_texpr (TyFun (arg_type, return_type)))
  | App _ -> "App"
  | Match _ -> "Match"
  | Tuple (_, ty, _) -> sprintf "Tuple : %s" (pp_texpr ty)
  | Let (_, _, x, _, _) -> sprintf "Let %s" x
  | LetRec (_, _, x, _, _) -> sprintf "LetRec %s" x
  | Constr (_, ty, cname) -> sprintf "Constructor %s : %s" cname (pp_texpr ty)
  | Seq _ -> "Seq"

let string_of_pat_node =
  let open Printf in
  function
  | Pat_Int (_, i) -> sprintf "Pat_Int %ld" i
  | Pat_Ident (_, ty, ident) -> sprintf "Pat_Ident %s : %s" ident (pp_texpr ty)
  | Pat_Bool (_, b) -> sprintf "Pat_Bool %b" b
  | Pat_Unit _ -> sprintf "Pat_()"
  | Pat_Any _ -> "Pat_Any"
  | Pat_Or _ -> "Pat_Or"
  | Pat_Tuple _ -> "Pat_Tuple"
  | Pat_Constr (_, ty, cname, _) ->
      sprintf "Pat_Constr %s : %s" cname (pp_texpr ty)

let rec pp_pattern ?(indent = "") pat =
  let open Printf in
  let pp_node n = printf "%s└──%s\n" indent (string_of_pat_node n) in
  let pp_rec_pattern = pp_pattern ~indent:(indent ^ "   ") in
  match pat with
  | Pat_Int _ | Pat_Ident _ | Pat_Bool _ | Pat_Unit _ | Pat_Any _ -> pp_node pat
  | Pat_Or (_, _, pats) ->
      pp_node pat;
      List.iter pp_rec_pattern pats
  | Pat_Tuple (_, _, ps) ->
      pp_node pat;
      List.iter pp_rec_pattern ps
  | Pat_Constr (_, _, _, None) -> pp_node pat
  | Pat_Constr (_, _, _, Some p) ->
      pp_node pat;
      pp_rec_pattern p

let rec pp_expr ?(indent = "") expr =
  let open Printf in
  let pp_node n = printf "%s└──%s\n" indent (string_of_expr_node n) in
  let pp_rec_expr = pp_expr ~indent:(indent ^ "   ") in
  match expr with
  | Int _ | Float _ | String _ | Bool _ | Ident _ | Unit _ | Constr _ ->
      pp_node expr
  | Bop (_, _, e0, _, e1) ->
      pp_node expr;
      pp_rec_expr e0;
      pp_rec_expr e1
  | Uop (_, _, _, e) ->
      pp_node expr;
      pp_rec_expr e
  | If (_, _, e0, e1, e2) ->
      pp_node expr;
      pp_rec_expr e0;
      pp_rec_expr e1;
      pp_rec_expr e2
  | Fun (_, _, _, _, e) ->
      pp_node expr;
      pp_rec_expr e
  | App (_, _, e0, e1) ->
      pp_node expr;
      pp_rec_expr e0;
      pp_rec_expr e1
  | Match (_, _, e0, cases) ->
      pp_node expr;
      pp_rec_expr e0;
      List.iter (pp_case (indent ^ "   ")) cases
  | Tuple (_, _, es) ->
      pp_node expr;
      List.iter pp_rec_expr es
  | Let (_, _, _, e0, e1) ->
      pp_node expr;
      pp_rec_expr e0;
      pp_rec_expr e1
  | LetRec (_, _, _, e0, e1) ->
      pp_node expr;
      pp_rec_expr e0;
      pp_rec_expr e1
  | Seq (_, _, es) ->
      pp_node expr;
      List.iter pp_rec_expr es

and pp_case indent (pattern, expr) =
  let open Printf in
  printf "%s└── <case>\n" indent;
  let indent' = indent ^ "   " in
  pp_pattern ~indent:indent' pattern;
  pp_expr ~indent:indent' expr

let pp_tconstr ?(indent = "") =
  let open Printf in
  function
  | DeclConstr (_, cname, None) -> printf "%s└──%s\n" indent cname
  | DeclConstr (_, cname, Some texpr) ->
      printf "%s└──%s of %s\n" indent cname (pp_texpr texpr)

let pp_decl ?(indent = "") =
  let open Printf in
  let print_with_indent = printf "%s└──%s\n" in
  function
  | Val (_, _, v, e) ->
      print_with_indent indent ("Val " ^ v);
      pp_expr ~indent:(indent ^ "   ") e
  | ValRec (_, _, v, e) ->
      print_with_indent indent ("ValRec " ^ v);
      pp_expr ~indent:(indent ^ "   ") e
  | Type (_, _, params, t, constructors) ->
      print_with_indent indent ("Type " ^ t);
      print_with_indent (indent ^ "   ")
        (String.concat "," params |> sprintf "params = [%s]");
      print_with_indent (indent ^ "   ") "constructors";
      List.iter (pp_tconstr ~indent:(indent ^ "      ")) constructors
