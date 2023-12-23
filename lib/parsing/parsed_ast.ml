open Common

type loc = Lexing.position * Lexing.position

type expr =
  | Int of loc * int
  | Ident of loc * string
  | Constr of loc * string
  | Bool of loc * bool
  | Unit of loc
  | Oper of loc * expr * oper * expr
  | If of loc * expr * expr * expr
  | Fun of loc * string * expr
  | App of loc * expr * expr
  | Match of loc * expr * (pattern * expr) list
  | Tuple of loc * expr list
  | Let of loc * string * expr * expr

and pattern =
  | Pat_Int of loc * int
  | Pat_Ident of loc * string
  | Pat_Bool of loc * bool
  | Pat_Unit of loc
  | Pat_Any of loc
  | Pat_Or of loc * pattern * pattern
  | Pat_Tuple of loc * pattern list
  | Pat_Constr of loc * string * pattern option

and type_expr =
  | TyInt
  | TyBool
  | TyUnit
  | TyCustom of string
  | TyVar of string
  | TyTuple of type_expr list
  | TyFun of type_expr * type_expr

and type_constr = DeclConstr of loc * string * type_expr option

and decl =
  | Val of loc * string * expr
  | Type of loc * string list * string * type_constr list

let string_of_expr_node =
  let open Printf in
  function
  | Int (_, i) -> sprintf "Int %i" i
  | Bool (_, b) -> sprintf "Bool %b" b
  | Ident (_, ident) -> sprintf "Ident %s" ident
  | Unit _ -> "()"
  | Constr (_, n) -> sprintf "Constructor %s" n
  | Oper (_, _, op, _) -> sprintf "Oper: %s" (show_oper op)
  | If _ -> "If"
  | Fun (_, x, _) -> sprintf "Fun %s" x
  | App _ -> "App"
  | Match _ -> "Match"
  | Tuple _ -> "Tuple"
  | Let (_, x, _, _) -> sprintf "Let %s" x

let string_of_pat_node =
  let open Printf in
  function
  | Pat_Int (_, i) -> sprintf "Pat_Int %i" i
  | Pat_Ident (_, ident) -> sprintf "Pat_Ident %s" ident
  | Pat_Bool (_, b) -> sprintf "Pat_Bool %b" b
  | Pat_Unit _ -> sprintf "Pat_()"
  | Pat_Any _ -> "Pat_Any"
  | Pat_Or _ -> "Pat_Or"
  | Pat_Tuple _ -> "Pat_Tuple"
  | Pat_Constr _ -> "Pat_Constr"

let rec pp_pattern ?(indent = "") pat =
  let open Printf in
  let pp_node n = printf "%s└──%s\n" indent (string_of_pat_node n) in
  let pp_rec_pattern = pp_pattern ~indent:(indent ^ "   ") in
  match pat with
  | Pat_Int _ | Pat_Ident _ | Pat_Bool _ | Pat_Unit _ | Pat_Any _ | Pat_Constr _
    ->
      pp_node pat
  | Pat_Or (_, p1, p2) ->
      pp_node pat;
      pp_rec_pattern p1;
      pp_rec_pattern p2
  | Pat_Tuple (_, ps) ->
      pp_node pat;
      List.iter pp_rec_pattern ps

let rec pp_expr ?(indent = "") expr =
  let open Printf in
  let pp_node n = printf "%s└──%s\n" indent (string_of_expr_node n) in
  let pp_rec_expr = pp_expr ~indent:(indent ^ "   ") in
  match expr with
  | Int _ | Bool _ | Ident _ | Unit _ | Constr _ -> pp_node expr
  | Oper (_, e0, _, e1) ->
      pp_node expr;
      pp_rec_expr e0;
      pp_rec_expr e1
  | If (_, e0, e1, e2) ->
      pp_node expr;
      pp_rec_expr e0;
      pp_rec_expr e1;
      pp_rec_expr e2
  | Fun (_, _, e) ->
      pp_node expr;
      pp_rec_expr e
  | App (_, e0, e1) ->
      pp_node expr;
      pp_rec_expr e0;
      pp_rec_expr e1
  | Match (_, e0, cases) ->
      pp_node expr;
      pp_rec_expr e0;
      List.iter (pp_case (indent ^ "   ")) cases
  | Tuple (_, es) ->
      pp_node expr;
      List.iter pp_rec_expr es
  | Let (_, _, e0, e1) ->
      pp_node expr;
      pp_rec_expr e0;
      pp_rec_expr e1

and pp_case indent (pattern, expr) =
  let open Printf in
  printf "%s└── <case>\n" indent;
  let indent' = indent ^ "   " in
  pp_pattern ~indent:indent' pattern;
  pp_expr ~indent:indent' expr

let rec pp_texpr = function
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyUnit -> "unit"
  | TyVar v | TyCustom v -> v
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
  | Val (_, v, e) ->
      print_with_indent indent ("Val " ^ v);
      pp_expr ~indent:(indent ^ "   ") e
  | Type (_, params, t, constructors) ->
      print_with_indent indent ("Type " ^ t);
      print_with_indent (indent ^ "   ")
        (sprintf "params = [%s]" (String.concat "," params));
      print_with_indent (indent ^ "   ") "constructors";
      List.iter (pp_tconstr ~indent:(indent ^ "      ")) constructors

(*
let rec pp_pattern = function
  | Pat_Int (_, i) -> string_of_int i
  | Pat_Bool (_, b) -> string_of_bool b
  | Pat_Ident (_, ident) -> ident
  | Pat_Unit _ -> "()"
  | Pat_Any _ -> "_"
  | Pat_Or (_, p1, p2) -> Printf.sprintf "%s | %s" (pp_pattern p1) (pp_pattern p2)
  | Pat_Tuple (_, pats) ->
      Printf.sprintf "(%s)" (List.map pp_pattern pats |> String.concat ",")
  | Pat_Constr (_, cname, None) -> Printf.sprintf "%s" cname
  | Pat_Constr (_, cname, Some p) -> Printf.sprintf "%s %s" cname (pp_pattern p)

let rec pp_case (p, e) = Printf.sprintf "[%s -> %s]" (pp_pattern p) (pp_expr e)

and pp_expr = function
  | Int (_, i) -> string_of_int i
  | Bool (_, b) -> string_of_bool b
  | Ident (_, ident) -> ident
  | Unit _ -> "()"
  | Constr (_, n) -> n
  | Oper (_, e1, op, e2) ->
      Printf.sprintf "{%s %s %s}" (pp_expr e1) (string_of_oper op) (pp_expr e2)
  | If (_, e1, e2, e3) ->
      Printf.sprintf "[if %s then %s else %s]" (pp_expr e1) (pp_expr e2)
        (pp_expr e3)
  | Fun (_, i, e) -> Printf.sprintf "[fun %s -> %s]" i (pp_expr e)
  | App (_, e1, e2) -> Printf.sprintf "(%s) %s" (pp_expr e1) (pp_expr e2)
  | Match (_, e, cl) ->
      Printf.sprintf "[match %s with %s]" (pp_expr e)
        (List.map pp_case cl |> String.concat " | ")
  | Tuple (_, es) -> Printf.sprintf "(%s)" (List.map pp_expr es |> String.concat ",")
  | Let (_, x, e1, e2) ->
      Printf.sprintf "let %s = %s in %s" x (pp_expr e1) (pp_expr e2)
(* | _ -> raise @@ Invalid_argument "printing not supported for this constructor!" *)

let rec pp_texpr = function
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyUnit -> "unit"
  | TyCustom s -> s
  | TyVar s -> "\'" ^ s
  | TyTuple ts ->
      List.map pp_texpr ts |> String.concat "*" |> Printf.sprintf "(%s)"
  | TyFun (t1, t2) -> Printf.sprintf "%s -> %s" (pp_texpr t1) (pp_texpr t2)

let pp_tconstr = function
  | DeclConstr (_, s, None) -> s
  | DeclConstr (_, s, Some texpr) -> Printf.sprintf "%s of %s" s (pp_texpr texpr)

let pp_decl = function
  | Val (_, v, e) -> Printf.sprintf "%s = %s" v (pp_expr e)
  | Type (_, params, t, tconstrs) ->
      Printf.sprintf "type %s %s = %s"
        (List.map (fun p -> "\'" ^ p) params
        |> String.concat "," |> Printf.sprintf "(%s)")
        t
        (List.map pp_tconstr tconstrs |> String.concat " | ")
*)
