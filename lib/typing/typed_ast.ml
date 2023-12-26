open Common

type loc = Lexing.position * Lexing.position

type expr =
  | Int of loc * int
  | Ident of loc * type_expr * string
  | Bool of loc * bool
  | Unit of loc
  | Bop of loc * type_expr * expr * Common.bop * expr
  | If of loc * type_expr * expr * expr * expr
  | Fun of loc * type_expr * string * expr
  | App of loc * type_expr * expr * expr
  | Match of loc * type_expr * expr * (Parsing.Parsed_ast.pattern * expr) list
  | Tuple of loc * type_expr * expr list
  | Let of loc * type_expr * string * expr * expr

(*
and pattern =
  | Pat_Int of int
  | Pat_Ident of string
  | Pat_Bool of bool
  | Pat_Unit
  | Pat_Any
  | Pat_Or of pattern * pattern
  | Pat_Tuple of type_expr * pattern list
  | Pat_Constr of type_expr * string * pattern option
*)
and type_constr = DeclConstr of loc * string * type_expr option

and decl =
  | Val of loc * type_expr * string * expr
  | Type of loc * string list * string * type_constr list

let bop_arg_type =
  let open Common in
  function
  | ADD | SUB | MUL | DIV | LT | GT -> TyInt
  | AND | OR -> TyBool
  | EQ -> raise (Invalid_argument "idk about eq yet - polymorphic?")

let bop_return_type =
  let open Common in
  function
  | ADD | SUB | MUL | DIV -> TyInt
  | AND | OR | LT | GT -> TyBool
  | EQ -> raise (Invalid_argument "idk about eq yet - polymorphic?")

(* printing *)
let string_of_expr_node =
  let open Printf in
  function
  | Int (_, i) -> sprintf "Int %i" i
  | Bool (_, b) -> sprintf "Bool %b" b
  | Ident (_, _, ident) -> sprintf "Ident %s" ident
  | Unit _ -> "()"
  (*| Constr (_, _, _, n) -> sprintf "Constructor %s" n *)
  | Bop (_, _, _, op, _) -> sprintf "Bop: %s" (show_bop op)
  | If _ -> "If"
  | Fun (_, _, x, _) -> sprintf "Fun %s" x
  | App _ -> "App"
  | Match _ -> "Match"
  | Tuple _ -> "Tuple"
  | Let (_, _, x, _, _) -> sprintf "Let %s" x

let string_of_pat_node = Parsing.Parsed_ast.string_of_pat_node
let pp_pattern = Parsing.Parsed_ast.pp_pattern

let rec pp_expr ?(indent = "") expr =
  let open Printf in
  let pp_node n = printf "%s└──%s\n" indent (string_of_expr_node n) in
  let pp_rec_expr = pp_expr ~indent:(indent ^ "   ") in
  match expr with
  | Int _ | Bool _ | Ident _ | Unit _ (*| Constr _ *) -> pp_node expr
  | Bop (_, _, e0, _, e1) ->
      pp_node expr;
      pp_rec_expr e0;
      pp_rec_expr e1
  | If (_, _, e0, e1, e2) ->
      pp_node expr;
      pp_rec_expr e0;
      pp_rec_expr e1;
      pp_rec_expr e2
  | Fun (_, _, _, e) ->
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
  | Type (_, params, t, constructors) ->
      print_with_indent indent ("Type " ^ t);
      print_with_indent (indent ^ "   ")
        (List.map (fun x -> "'" ^ x) params
        |> String.concat "," |> sprintf "params = [%s]");
      print_with_indent (indent ^ "   ") "constructors";
      List.iter (pp_tconstr ~indent:(indent ^ "      ")) constructors
