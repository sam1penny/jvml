open Instruction
open Common

type source =
  | Local_var of int
  | Instance_field of string * type_expr
  | Static_field of string * string * type_expr

type env = {
  fields : source StringMap.t;
  static_methods : (string * int * type_expr list * type_expr) StringMap.t;
}

let empty = StringMap.empty

let stdlib =
  {
    fields =
      StringMap.singleton "print_$0"
        (Static_field ("Std", "print", TyFun (TyAny, TyUnit)));
    static_methods = StringMap.empty;
  }

let add_local_var k v env =
  { env with fields = StringMap.add k (Local_var v) env.fields }

let add_instance_field k (f, ty) env =
  { env with fields = StringMap.add k (Instance_field (f, ty)) env.fields }

let add_static_field k (clazz, f, ty) env =
  { env with fields = StringMap.add k (Static_field (clazz, f, ty)) env.fields }

let lookup k env =
  match StringMap.find_opt k env.fields with
  | Some (Local_var v) -> [ LOAD_REF v ]
  | Some (Instance_field (f, ty)) -> [ LOAD_REF 0; LOAD_FIELD (f, ty) ]
  | Some (Static_field (clazz, name, ty)) -> [ LOAD_STATIC (clazz, name, ty) ]
  | None ->
      raise
      @@ Failure
           (Printf.sprintf
              "looked up unknown var/field/staticmethod '%s' when lowering to \
               linear_ir"
              k)

let strip_nonstatic env =
  {
    env with
    fields =
      StringMap.filter
        (fun _ -> function Static_field _ -> true | _ -> false)
        env.fields;
  }

let add_static_method name (gen_name, num_args, ty_args, ty_ret) env =
  {
    env with
    static_methods =
      StringMap.add name
        (gen_name, num_args, ty_args, ty_ret)
        env.static_methods;
  }

let lookup_static_method name env = StringMap.find_opt name env.static_methods
