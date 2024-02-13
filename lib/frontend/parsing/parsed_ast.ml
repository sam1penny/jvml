open Common

type loc = Lexing.position * Lexing.position

type expr =
  | Int of loc * int
  | Ident of loc * string
  | Constr of loc * string
  | Bool of loc * bool
  | Unit of loc
  | Bop of loc * expr * bop * expr
  | If of loc * expr * expr * expr
  | Fun of loc * string * expr
  | App of loc * expr * expr
  | Match of loc * expr * (pattern * expr) list
  | Tuple of loc * expr list
  | Let of loc * string * expr * expr
  | LetRec of loc * string * expr * expr
  | Seq of loc * expr list

and pattern =
  | Pat_Int of loc * int
  | Pat_Ident of loc * string
  | Pat_Bool of loc * bool
  | Pat_Unit of loc
  | Pat_Any of loc
  | Pat_Or of loc * pattern list
  | Pat_Tuple of loc * pattern list
  | Pat_Constr of loc * string * pattern option

type type_expr =
  | TyInt of loc
  | TyBool of loc
  | TyUnit of loc
  | TyCustom of loc * type_expr list * string
  | TyVar of loc * string
  | TyTuple of loc * type_expr list
  | TyFun of loc * type_expr * type_expr

type type_constr = DeclConstr of loc * string * type_expr option

type decl =
  | Val of loc * string * expr
  | ValRec of loc * string * expr
  | Type of loc * string list * string * type_constr list

let get_expr_loc = function
  | Int (loc, _) -> loc
  | Ident (loc, _) -> loc
  | Bool (loc, _) -> loc
  | Unit loc -> loc
  | Bop (loc, _, _, _) -> loc
  | If (loc, _, _, _) -> loc
  | Fun (loc, _, _) -> loc
  | App (loc, _, _) -> loc
  | Match (loc, _, _) -> loc
  | Tuple (loc, _) -> loc
  | Let (loc, _, _, _) -> loc
  | LetRec (loc, _, _, _) -> loc
  | Constr (loc, _) -> loc
  | Seq (loc, _) -> loc

let get_pattern_loc = function
  | Pat_Int (loc, _) -> loc
  | Pat_Ident (loc, _) -> loc
  | Pat_Bool (loc, _) -> loc
  | Pat_Unit loc -> loc
  | Pat_Any loc -> loc
  | Pat_Or (loc, _) -> loc
  | Pat_Tuple (loc, _) -> loc
  | Pat_Constr (loc, _, _) -> loc

let string_of_expr_node =
  let open Printf in
  function
  | Int (_, i) -> sprintf "Int %i" i
  | Bool (_, b) -> sprintf "Bool %b" b
  | Ident (_, ident) -> sprintf "Ident %s" ident
  | Unit _ -> "()"
  | Constr (_, n) -> sprintf "Constructor %s" n
  | Bop (_, _, op, _) -> sprintf "Bop: %s" (show_bop op)
  | If _ -> "If"
  | Fun (_, x, _) -> sprintf "Fun %s" x
  | App _ -> "App"
  | Match _ -> "Match"
  | Tuple _ -> "Tuple"
  | Let (_, x, _, _) -> sprintf "Let %s" x
  | LetRec (_, x, _, _) -> sprintf "LetRec %s" x
  | Seq _ -> "Seq"

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
  | Pat_Constr (_, cname, _) -> sprintf "Pat_Constr %s" cname

let rec pp_texpr = function
  | TyInt _ -> "int"
  | TyBool _ -> "bool"
  | TyUnit _ -> "unit"
  | TyVar (_, v) -> v
  | TyCustom (_, [], v) -> v
  | TyCustom (_, [ t ], v) -> Printf.sprintf "%s %s" (pp_texpr t) v
  | TyCustom (_, t :: ts, v) ->
      Printf.sprintf "(%s) %s"
        (List.map pp_texpr (t :: ts) |> String.concat ", ")
        v
  | TyTuple (_, ts) ->
      List.map
        (fun t ->
          match t with TyFun _ -> "(" ^ pp_texpr t ^ ")" | _ -> pp_texpr t)
        ts
      |> String.concat " * " |> Printf.sprintf "(%s)"
  | TyFun (_, f, c) -> (
      (match f with TyFun _ -> "(" ^ pp_texpr f ^ ")" | _ -> pp_texpr f)
      ^ " -> "
      ^ match c with TyFun _ -> "" ^ pp_texpr c ^ "" | _ -> pp_texpr c)

let rec pp_pattern ?(indent = "") pat =
  let open Printf in
  let pp_node n = printf "%s└──%s\n" indent (string_of_pat_node n) in
  let pp_rec_pattern = pp_pattern ~indent:(indent ^ "   ") in
  match pat with
  | Pat_Int _ | Pat_Ident _ | Pat_Bool _ | Pat_Unit _ | Pat_Any _ -> pp_node pat
  | Pat_Or (_, pats) ->
      pp_node pat;
      List.iter pp_rec_pattern pats
  | Pat_Tuple (_, ps) ->
      pp_node pat;
      List.iter pp_rec_pattern ps
  | Pat_Constr (_, _, None) -> pp_node pat
  | Pat_Constr (_, _, Some p) ->
      pp_node pat;
      pp_rec_pattern p

let rec pp_expr ?(indent = "") expr =
  let open Printf in
  let pp_node n = printf "%s└──%s\n" indent (string_of_expr_node n) in
  let pp_rec_expr = pp_expr ~indent:(indent ^ "   ") in
  match expr with
  | Int _ | Bool _ | Ident _ | Unit _ | Constr _ -> pp_node expr
  | Bop (_, e0, _, e1) ->
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
  | Let (_, _, e0, e1) | LetRec (_, _, e0, e1) ->
      pp_node expr;
      pp_rec_expr e0;
      pp_rec_expr e1
  | Seq (_, es) ->
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
  | Val (_, v, e) ->
      print_with_indent indent ("Val " ^ v);
      pp_expr ~indent:(indent ^ "   ") e
  | ValRec (_, v, e) ->
      print_with_indent indent ("ValRec " ^ v);
      pp_expr ~indent:(indent ^ "   ") e
  | Type (_, params, t, constructors) ->
      print_with_indent indent ("Type " ^ t);
      print_with_indent (indent ^ "   ")
        (String.concat "," params |> sprintf "params = [%s]");
      print_with_indent (indent ^ "   ") "constructors";
      List.iter (pp_tconstr ~indent:(indent ^ "      ")) constructors
