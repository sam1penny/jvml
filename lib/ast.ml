type expr =
  | INT of int
  | ADD of expr * expr
  | SUB of expr * expr
  | MUL of expr * expr
  | DIV of expr * expr


let rec print_helper indent = function
  | INT i -> print_endline (indent ^ " " ^ (string_of_int i))
  | ADD (e1, e2) -> let new_indent = indent ^ "----" in
                    print_helper new_indent e1 ; print_endline (indent ^ " +") ; print_helper new_indent e2
  | SUB (e1, e2) -> let new_indent = indent ^ "----" in
                    print_helper new_indent e1 ; print_endline (indent ^ " -") ; print_helper new_indent e2
  | MUL (e1, e2) -> let new_indent = indent ^ "----" in
                    print_helper new_indent e1 ; print_endline (indent ^ " *") ; print_helper new_indent e2
  | DIV (e1, e2) -> let new_indent = indent ^ "----" in
                    print_helper new_indent e1 ; print_endline (indent ^ " /") ; print_helper new_indent e2

let pretty_print = print_helper ""