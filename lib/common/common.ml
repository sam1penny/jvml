type bop = ADD | SUB | MUL | DIV | EQ | LT | GT | AND | OR

let show_bop = function
  | ADD -> "+"
  | MUL -> "*"
  | DIV -> "/"
  | SUB -> "-"
  | LT -> "<"
  | GT -> ">"
  | EQ -> "="
  | AND -> "&&"
  | OR -> "||"

let pp_bop formatter bop = Format.fprintf formatter "@[%s@]" (show_bop bop)

type type_expr =
  | TyInt
  | TyBool
  | TyUnit
  | TyCustom of type_expr list * string
  | TyVar of string
  | TyTuple of type_expr list
  | TyFun of type_expr * type_expr

let rec pp_texpr = function
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyUnit -> "unit"
  | TyVar v -> v
  | TyCustom ([], v) -> v
  | TyCustom (t :: ts, v) ->
      Printf.sprintf "(%s) %s"
        (List.map pp_texpr (t :: ts) |> String.concat ",")
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

let ( >>=? ) x f = Result.bind x f
let ( >>= ) x f = Option.bind x f

let collect_result l =
  List.fold_right
    (fun x acc ->
      acc >>=? fun acc ->
      x >>=? fun x -> Ok (x :: acc))
    l (Ok [])
