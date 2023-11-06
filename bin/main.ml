open Jvml

let () = Driver.parse_file "examples/arithmetic.jvml" |> Ast.pretty_print