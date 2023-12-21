type oper = ADD | SUB | MUL | DIV | EQ | LT | GT | AND | OR

let ( >>=? ) x f = Result.bind x f
let ( >>= ) x f = Option.bind x f

let collect_result l =
  List.fold_right
    (fun x acc ->
      acc >>=? fun acc ->
      x >>=? fun x -> Ok (x :: acc))
    l (Ok [])
