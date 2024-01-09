open Instruction
module StringMap = Map.Make (String)

type env = { local_vars : string StringMap.t; fields : string StringMap.t }

let empty = { local_vars = StringMap.empty; fields = StringMap.empty }

let add_local_var k v env =
  { env with local_vars = StringMap.add k v env.local_vars }

let add_field k v env = { env with fields = StringMap.add k v env.fields }

let lookup k env =
  match StringMap.find_opt k env.local_vars with
  | Some b -> [ LOAD_REF b ]
  (* put 'this' on stack, then load field *)
  | None -> [ LOAD_REF "0"; LOAD_FIELD (StringMap.find k env.fields) ]
