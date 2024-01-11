open Instruction
open Typing.Typed_ast
module StringMap = Map.Make (String)

type env = {
  local_vars : string StringMap.t;
  instance_fields : (string * type_expr) StringMap.t;
  static_fields : (string * string * type_expr) StringMap.t;
}

let empty =
  {
    local_vars = StringMap.empty;
    instance_fields = StringMap.empty;
    static_fields = StringMap.empty;
  }

let stdlib =
  {
    local_vars = StringMap.empty;
    instance_fields = StringMap.empty;
    static_fields =
      StringMap.singleton "print" ("Std", "print", TyFun (TyVar "'a", TyUnit));
  }

let add_local_var k v env =
  { env with local_vars = StringMap.add k v env.local_vars }

let add_instance_field k v env =
  { env with instance_fields = StringMap.add k v env.instance_fields }

let add_static_field k v env =
  { env with static_fields = StringMap.add k v env.static_fields }

let lookup k env =
  match StringMap.find_opt k env.local_vars with
  | Some b -> [ LOAD_REF b ]
  (* put 'this' on stack, then load field *)
  | None -> (
      match StringMap.find_opt k env.instance_fields with
      | Some (f, ty) -> [ LOAD_REF "0"; LOAD_FIELD (f, ty) ]
      | None -> (
          match StringMap.find_opt k env.static_fields with
          | Some (clazz, name, ty) -> [ LOAD_STATIC (clazz, name, ty) ]
          | None ->
              raise
              @@ Failure
                   "looked up unknown var/field/staticmethod when lowering to \
                    linear_ir"))
