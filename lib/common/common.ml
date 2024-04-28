type bop =
  | ADD
  | SUB
  | MUL
  | DIV
  | EQ
  | LT
  | GT
  | LEQ
  | GEQ
  | FLOAT_LT
  | FLOAT_GT
  | FLOAT_LEQ
  | FLOAT_GEQ
  | AND
  | OR
  | FLOAT_ADD
  | FLOAT_SUB
  | FLOAT_MUL
  | FLOAT_DIV
  | STRING_CONCAT

type uop = NEG | FLOAT_NEG | REAL | NOT

let show_bop = function
  | ADD -> "+"
  | MUL -> "*"
  | DIV -> "/"
  | SUB -> "-"
  | LT -> "<"
  | GT -> ">"
  | LEQ -> "<="
  | GEQ -> ">="
  | FLOAT_LT -> "<."
  | FLOAT_GT -> ">."
  | FLOAT_LEQ -> "<=."
  | FLOAT_GEQ -> ">=."
  | EQ -> "="
  | AND -> "&&"
  | OR -> "||"
  | FLOAT_ADD -> "+."
  | FLOAT_SUB -> "-."
  | FLOAT_MUL -> "*."
  | FLOAT_DIV -> "/."
  | STRING_CONCAT -> "^"

let show_uop = function
  | NEG -> "-"
  | FLOAT_NEG -> "-."
  | REAL -> "real"
  | NOT -> "not"

let pp_bop formatter bop = Format.fprintf formatter "@[%s@]" (show_bop bop)
let ( >>=? ) x f = Result.bind x f
let ( >>= ) x f = Option.bind x f

let rec take list i =
  match list with
  | [] -> []
  | x :: xs -> if i = 0 then [] else x :: take xs (i - 1)

let rec drop list i =
  match list with [] -> [] | _ :: xs -> if i = 0 then xs else drop xs (i - 1)

(** Collect an ('a, 'b) result list into a ('a list, 'b) result.
    In case of multiple errors, the rightmost error is returned
*)
let collect_result l =
  List.fold_right
    (fun x acc ->
      acc >>=? fun acc ->
      x >>=? fun x -> Ok (x :: acc))
    l (Ok [])

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

module Hashset = struct
  include Hashtbl

  type 'a t = ('a, unit) Hashtbl.t

  let add set x = Hashtbl.add set x ()
  let iter f set = Hashtbl.iter (fun x _ -> f x) set
end

let debug_json = ref (Yojson.Basic.from_string "{}")

let update_json k v =
  match !debug_json with
  | `Assoc assoc_list ->
      let rec loop l =
        match l with
        | [] -> [ (k, v) ]
        | (k', v') :: xs -> if k = k' then (k, v) :: xs else (k', v') :: loop xs
      in
      debug_json := `Assoc (loop assoc_list)
  | _ -> raise @@ Failure "invalid json format"

let maybe_record_compiler_time compiler_phase program =
  (match !Config.debug_file with
  | None -> ()
  | Some _ ->
      update_json compiler_phase
        (Yojson.Basic.from_string (string_of_float @@ Sys.time ())));
  program

module Config = Config
