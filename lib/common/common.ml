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
let ( >>=? ) x f = Result.bind x f
let ( >>= ) x f = Option.bind x f

let collect_result l =
  List.fold_right
    (fun x acc ->
      acc >>=? fun acc ->
      x >>=? fun x -> Ok (x :: acc))
    l (Ok [])
