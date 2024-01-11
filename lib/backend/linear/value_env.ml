open Instruction
open Typing.Typed_ast
module StringMap = Map.Make (String)

type env = {
  local_vars : string StringMap.t;
  fields : (string * type_expr) StringMap.t;
  stdlib : type_expr StringMap.t;
}

let empty =
  {
    local_vars = StringMap.empty;
    fields = StringMap.empty;
    stdlib = StringMap.empty;
  }

let stdlib =
  {
    local_vars = StringMap.empty;
    fields = StringMap.empty;
    stdlib = StringMap.singleton "print" (TyFun (TyVar "'a", TyUnit));
  }

let add_local_var k v env =
  { env with local_vars = StringMap.add k v env.local_vars }

let add_field k v env = { env with fields = StringMap.add k v env.fields }

let lookup k env =
  match StringMap.find_opt k env.local_vars with
  | Some b -> [ LOAD_REF b ]
  (* put 'this' on stack, then load field *)
  | None -> (
      match StringMap.find_opt k env.fields with
      | Some (f, ty) -> [ LOAD_REF "0"; LOAD_FIELD (f, ty) ]
      | None -> (
          match StringMap.find_opt k env.stdlib with
          | Some ty -> [ LOAD_STD (k, ty) ]
          | None ->
              raise
              @@ Failure
                   "looked up unknown var/field/staticmethod when lowering to \
                    linear_ir"))
