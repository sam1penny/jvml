open Instruction
open Typing.Typed_ast
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

type label_generators = {
  ctrl_label : unit -> string;
  ref_label : unit -> string;
  lambda_label : unit -> string;
}

let make_counter () =
  let t = ref 1 in
  fun () ->
    let n = !t in
    let _ = t := !t + 1 in
    string_of_int n

let make_ctrl_gen () =
  let cnt = make_counter () in
  fun () -> "L" ^ cnt ()

let make_lambda_gen () =
  let cnt = make_counter () in
  fun () -> "Lambda$" ^ cnt ()

let make_generators () =
  {
    ctrl_label = make_ctrl_gen ();
    ref_label = make_counter ();
    lambda_label = make_lambda_gen ();
  }

let reset_per_func_generators g =
  { g with ctrl_label = make_ctrl_gen (); ref_label = make_counter () }

let rec pattern_bindings = function
  | Pat_Int _ | Pat_Bool _ | Pat_Unit _ | Pat_Any _ -> StringSet.empty
  | Pat_Ident (_, _, i) -> StringSet.singleton i
  | Pat_Or (_, _, p0, p1) ->
      StringSet.union (pattern_bindings p0) (pattern_bindings p1)
  | Pat_Tuple (_, _, ps) ->
      List.map pattern_bindings ps
      |> List.fold_left StringSet.union StringSet.empty
  | Pat_Constr (_, _, _, None) -> StringSet.empty
  | Pat_Constr (_, _, _, Some p) -> pattern_bindings p

let free_vars_with_types_expr bound e =
  let takeleft _ x _ = Some x in
  let rec aux bound free = function
    | Int _ | Bool _ | Unit _ | Constr _ -> free
    | Ident (_, ty, i) ->
        if StringSet.mem i bound then free else StringMap.add i ty free
    | Bop (_, _, e0, _, e1) ->
        StringMap.union takeleft (aux bound free e0) (aux bound free e1)
    | If (_, _, e0, e1, e2) ->
        StringMap.union takeleft (aux bound free e0) (aux bound free e1)
        |> StringMap.union takeleft (aux bound free e2)
    | Fun (_, _, _, x, e) ->
        let bound' = StringSet.add x bound in
        aux bound' free e
    | App (_, _, e0, e1) ->
        StringMap.union takeleft (aux bound free e0) (aux bound free e1)
    | Match (_, _, e, cases) ->
        let free_e = aux bound free e in
        let free_cases =
          List.map
            (fun (p, ce) ->
              let bound' = StringSet.union bound (pattern_bindings p) in
              aux bound' free ce)
            cases
        in
        List.fold_left (StringMap.union takeleft) StringMap.empty
          (free_e :: free_cases)
    | Tuple (_, _, es) ->
        List.map (aux bound free) es
        |> List.fold_left (StringMap.union takeleft) StringMap.empty
    | Let (_, _, x, e0, e1) ->
        let free_e0 = aux bound free e0 in
        let bound' = StringSet.add x bound in
        StringMap.union takeleft free_e0 (aux bound' free e1)
  in
  aux bound StringMap.empty e

let rec compile_expr label_gen env e =
  match e with
  | Int (_, i) -> ([], [ PUSH_INT i; BOX_INT ])
  | Bool (_, b) -> ([], [ PUSH_INT (if b then 1 else 0); BOX_INT ])
  | Ident (_, _, x) -> ([], Value_env.lookup x env)
  | Bop (_, _, e0, bop, e1) -> compile_bop label_gen env e0 e1 bop
  | If (_, _, e0, e1, e2) ->
      let else_label = label_gen.ctrl_label () in
      let after_label = label_gen.ctrl_label () in
      let defs0, c0 = compile_expr label_gen env e0 in
      let defs1, c1 = compile_expr label_gen env e1 in
      let defs2, c2 = compile_expr label_gen env e2 in
      ( defs0 @ defs1 @ defs2,
        c0
        @ [ UNBOX_INT; IFZERO else_label ]
        @ c1
        @ [ GOTO after_label; LABEL else_label ]
        @ c2 @ [ LABEL after_label ] )
  | Fun (_, t0, t1, x, e) -> compile_lambda label_gen env (t0, t1, x, e)
  | App (_, ty, e0, e1) ->
      let defs0, c0 = compile_expr label_gen env e0 in
      let defs1, c1 = compile_expr label_gen env e1 in
      (defs0 @ defs1, c0 @ c1 @ [ APPLY ty ])
  | Let (_, _, x, e0, e1) ->
      let defs0, c0 = compile_expr label_gen env e0 in
      let x_label = label_gen.ref_label () in
      let env_with_x = Value_env.add_local_var x x_label env in
      let defs1, c1 = compile_expr label_gen env_with_x e1 in
      (defs0 @ defs1, c0 @ [ STORE_REF x_label ] @ c1)
  | _ ->
      raise
      @@ Invalid_argument "Attempted to lower unsupported expr to linear_ir"

and compile_bop label_gen env e0 e1 = function
  | (ADD | SUB | MUL | DIV | LT | GT) as intop ->
      let defs0, c0 = compile_expr label_gen env e0 in
      let defs1, c1 = compile_expr label_gen env e1 in
      ( defs0 @ defs1,
        c0 @ [ UNBOX_INT ] @ c1 @ [ UNBOX_INT ] @ [ BOP intop; BOX_INT ] )
  (* polymorphic eq *)
  | EQ ->
      let defs0, c0 = compile_expr label_gen env e0 in
      let defs1, c1 = compile_expr label_gen env e1 in
      (defs0 @ defs1, c0 @ c1 @ [ BOP EQ ])
  | _ ->
      raise
      @@ Invalid_argument
           "Attempted to lower unsupported binary operator to linear_ir"

and compile_lambda label_gen env (arg_type, return_type, x, e) =
  let fvars_with_types =
    free_vars_with_types_expr (StringSet.singleton x) e
    |> StringMap.to_seq |> List.of_seq
  in
  let fvars = List.map (fun (x, _) -> x) fvars_with_types in
  let fvar_types = List.map (fun (_, y) -> y) fvars_with_types in
  let fetch_fvars = List.map (fun x -> Value_env.lookup x env) fvars in

  let closure_label = label_gen.lambda_label () in
  let body_label_gen = reset_per_func_generators label_gen in

  let body_env =
    Value_env.empty |> Value_env.add_local_var x (body_label_gen.ref_label ())
  in
  let body_env =
    List.fold_left
      (fun acc (name, ty) -> Value_env.add_field name (name, ty) acc)
      body_env fvars_with_types
  in
  let defs, ecode = compile_expr body_label_gen body_env e in
  let closure =
    CLOSURE (closure_label, fvars_with_types, arg_type, return_type, ecode)
  in
  ( defs @ [ closure ],
    [ ALLOC_CLOSURE closure_label ]
    @ (List.rev fetch_fvars |> List.flatten)
    @ [ CONSTRUCT_CLOSURE (closure_label, List.rev fvar_types) ] )

let compile_decl label_gen env = function
  | Val (_, _, x, e) ->
      let defs, c = compile_expr label_gen env e in
      let x_label = label_gen.ref_label () in
      (defs, c @ [ STORE_REF x_label ], Value_env.add_local_var x x_label env)
  | _ ->
      raise
      @@ Invalid_argument
           "Attempted to lower unsupported declaration to linear_ir"

let compile_decl_from_scratch d =
  let generators = make_generators () in
  let defs, code, _ = compile_decl generators Value_env.empty d in
  (defs, code)

let compile_program_from_scratch p =
  let generators = make_generators () in
  List.fold_left
    (fun (defsacc, codeacc, envacc) d ->
      let defs, code, env = compile_decl generators envacc d in
      (defsacc @ defs, codeacc @ code, env))
    ([], [], Value_env.empty) p
  |> fun (defs, code, _) -> (defs, code)
