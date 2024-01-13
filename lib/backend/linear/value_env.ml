open Instruction
module StringMap = Map.Make (String)

type source =
  | Local_var of string
  | Instance_field of string * type_expr
  | Static_field of string * string * type_expr

type env = source StringMap.t

let empty = StringMap.empty

let stdlib =
  StringMap.singleton "print"
    (Static_field ("Std", "print", TyFun (TyAny, TyUnit)))

let add_local_var k v env = StringMap.add k (Local_var v) env

let add_instance_field k (f, ty) env =
  StringMap.add k (Instance_field (f, ty)) env

let add_static_field k (clazz, f, ty) env =
  StringMap.add k (Static_field (clazz, f, ty)) env

let lookup k env =
  match StringMap.find_opt k env with
  | Some (Local_var v) -> [ LOAD_REF v ]
  | Some (Instance_field (f, ty)) -> [ LOAD_REF "0"; LOAD_FIELD (f, ty) ]
  | Some (Static_field (clazz, name, ty)) -> [ LOAD_STATIC (clazz, name, ty) ]
  | None ->
      raise
      @@ Failure
           "looked up unknown var/field/staticmethod when lowering to linear_ir"
