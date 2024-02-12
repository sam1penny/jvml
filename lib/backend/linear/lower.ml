open Instruction
open Common
open Typing
open Desugar
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

type label_generators = {
  ctrl_label : unit -> string;
  ref_label : unit -> int;
  lambda_label : unit -> string;
  static_label : unit -> string;
}

let make_counter n =
  let t = ref n in
  fun () ->
    let n = !t in
    let _ = t := !t + 1 in
    n

let make_ctrl_gen () =
  let cnt = make_counter 0 in
  fun () -> "L" ^ string_of_int @@ cnt ()

let make_lambda_gen () =
  let cnt = make_counter 0 in
  fun () -> "Lambda$" ^ string_of_int @@ cnt ()

let make_static_gen () =
  let cnt = make_counter 0 in
  fun () -> "SF" ^ string_of_int @@ cnt ()

let make_generators () =
  {
    ctrl_label = make_ctrl_gen ();
    ref_label = make_counter 1;
    lambda_label = make_lambda_gen ();
    static_label = make_static_gen ();
  }

let reset_per_func_generators g =
  { g with ctrl_label = make_ctrl_gen (); ref_label = make_counter 1 }

let reset_for_static_func_generators g =
  { g with ctrl_label = make_ctrl_gen (); ref_label = make_counter 0 }

let rec intersperse sep = function
  | ([] | [ _ ]) as l -> l
  | x :: xs -> x :: sep :: intersperse sep xs

let free_vars_with_types_expr bound e =
  let open Desugared_ast in
  let takeleft _ x _ = Some x in
  let rec aux bound free = function
    | Int _ | Bool _ | Unit | Constr _ -> free
    | Ident (ty, i) ->
        if StringSet.mem i bound then free else StringMap.add i ty free
    | Bop (_, e0, _, e1) ->
        StringMap.union takeleft (aux bound free e0) (aux bound free e1)
    | If (_, e0, e1, e2) ->
        StringMap.union takeleft (aux bound free e0) (aux bound free e1)
        |> StringMap.union takeleft (aux bound free e2)
    | Fun (_, _, x, e) ->
        let bound' = StringSet.add x bound in
        aux bound' free e
    | App (_, e0, e1) ->
        StringMap.union takeleft (aux bound free e0) (aux bound free e1)
    | Tuple (_, es) ->
        List.map (aux bound free) es
        |> List.fold_left (StringMap.union takeleft) StringMap.empty
    | Let (_, x, e0, e1) ->
        let free_e0 = aux bound free e0 in
        let bound' = StringSet.add x bound in
        StringMap.union takeleft free_e0 (aux bound' free e1)
    | LetRec (_, x, e0, e1) ->
        let bound' = StringSet.add x bound in
        StringMap.union takeleft (aux bound' free e0) (aux bound' free e1)
    | Seq (_, es) ->
        List.map (aux bound free) es
        |> List.fold_left (StringMap.union takeleft) StringMap.empty
    (* index is undecidable, so look at entire e *)
    | TupleGet (_, _, e) -> aux bound free e
    | ConstructorGet (_, _, e) -> aux bound free e
    | Switch (_, e, cases, fallback_opt) ->
        let free_e = aux bound free e in
        List.map (fun (_, case_expr) -> case_expr) cases
        |> List.map (aux bound free)
        |> List.fold_left (StringMap.union takeleft) free_e
        |> StringMap.union takeleft
             (Option.map (fun e -> aux bound free e) fallback_opt
             |> Option.value ~default:StringMap.empty)
    | Match_Failure -> StringMap.empty
  in

  aux bound StringMap.empty e

let rec convert_type = function
  | Typed_ast.TyInt -> Instruction.TyInt
  | Typed_ast.TyBool -> Instruction.TyBool
  | Typed_ast.TyUnit -> Instruction.TyUnit
  | Typed_ast.TyVar _ -> Instruction.TyAny
  | Typed_ast.TyCustom (_, n) -> Instruction.TyCustom n
  | Typed_ast.TyFun (t0, t1) ->
      Instruction.TyFun (convert_type t0, convert_type t1)
  | Typed_ast.TyTuple ts -> Instruction.TyTuple (List.map convert_type ts)

let con_index = function
  | Desugared_ast.IntCon i -> i
  | Desugared_ast.AdtCon (_, tag) -> tag

let get_index = function
  | Typed_ast.TyInt -> [ UNBOX_INT ]
  | Typed_ast.TyBool -> [ UNBOX_BOOL ]
  (*| Typed_ast.TyUnit -> [PUSH_INT 0; UNBOX_INT]*)
  | Typed_ast.TyCustom (_, tname) -> [ CONSTRUCTOR_INDEX tname ]
  | _ -> raise @@ Failure "attempted to match on unsupported type"

let rec compile_expr label_gen env top_level_bindings e =
  let open Desugared_ast in
  (* curried for convenience *)
  let compile_expr_rec = compile_expr label_gen env top_level_bindings in
  match e with
  | Int i -> ([], [ PUSH_INT i; BOX_INT ], [])
  | Bool b -> ([], [ PUSH_BOOL b; BOX_BOOL ], [])
  | Ident (_, x) -> ([], Value_env.lookup x env, [])
  | Unit -> ([], [ PUSH_UNIT ], [])
  | Bop (_, e0, bop, e1) ->
      compile_bop label_gen env top_level_bindings e0 e1 bop
  | If (_, e0, e1, e2) ->
      let else_label = label_gen.ctrl_label () in
      let after_label = label_gen.ctrl_label () in
      let defs0, c0, s0 = compile_expr_rec e0 in
      let defs1, c1, s1 = compile_expr_rec e1 in
      let defs2, c2, s2 = compile_expr_rec e2 in
      ( defs0 @ defs1 @ defs2,
        c0
        @ [ UNBOX_BOOL; IFZERO else_label ]
        @ c1
        @ [ GOTO after_label; LABEL else_label ]
        @ c2 @ [ LABEL after_label ],
        s0 @ s1 @ s2 )
  | Fun (t0, t1, x, e) ->
      compile_anon_lambda_expr label_gen env top_level_bindings
        (convert_type t0, convert_type t1, x, e)
  | App (ty, e0, e1) ->
      let defs0, c0, s0 = compile_expr_rec e0 in
      let defs1, c1, s1 = compile_expr_rec e1 in
      (defs0 @ defs1, c0 @ c1 @ [ APPLY (convert_type ty) ], s0 @ s1)
  | Let (_, x, e0, e1) ->
      let defs0, c0, s0 = compile_expr_rec e0 in
      let x_label = label_gen.ref_label () in
      let env_with_x = Value_env.add_local_var x x_label env in
      let defs1, c1, s1 =
        compile_expr label_gen env_with_x top_level_bindings e1
      in
      (defs0 @ defs1, c0 @ [ STORE_REF x_label ] @ c1, s0 @ s1)
  | LetRec (_, x, e0, e1) ->
      let x_label = label_gen.static_label () in
      let env_with_x =
        Value_env.add_static_field x
          ("Foo", x_label, convert_type (Desugared_ast.get_expr_type e0))
          env
      in
      let new_toplevel = StringSet.add x top_level_bindings in
      let defs0, c0, s0 = compile_expr label_gen env_with_x new_toplevel e0 in
      let defs1, c1, s1 =
        compile_expr label_gen env_with_x top_level_bindings e1
      in
      ( defs0 @ defs1,
        c0
        @ [
            STORE_STATIC
              ("Foo", x_label, convert_type (Desugared_ast.get_expr_type e0));
          ]
        @ c1,
        s0 @ s1 )
  | Constr (_, cname) -> ([], Value_env.lookup cname env, [])
  | Tuple (_, ts) ->
      let defs, lowered_tuple_code, smethods =
        lower_tuple_eles_to_array label_gen env top_level_bindings ts
      in
      ( defs,
        [ ALLOC_OBJ "Tuple" ] @ lowered_tuple_code
        @ [ CONSTRUCT_OBJ ("Tuple", [ TyArray TyAny ]) ],
        smethods )
  | Seq (_, es) ->
      let linear_es = List.map compile_expr_rec es in
      let defs =
        List.map (fun (defs, _, _) -> defs) linear_es |> List.flatten
      in
      let ecode = List.map (fun (_, code, _) -> code) linear_es in
      let smethods =
        List.map (fun (_, _, smethods) -> smethods) linear_es |> List.flatten
      in
      let pop_throwaways = intersperse [ POP ] ecode in
      (defs, List.flatten pop_throwaways, smethods)
  | TupleGet (ty, i, e) ->
      let defs, code, static_methods = compile_expr_rec e in
      (defs, code @ [ TUPLE_GET (convert_type ty, i) ], static_methods)
  | ConstructorGet (ty, cname, e) ->
      let defs, code, static_methods = compile_expr_rec e in
      (defs, code @ [ CONSTRUCTOR_GET (convert_type ty, cname) ], static_methods)
  | Switch (_, e0, cases, fallback_opt) -> (
      let defs0, code0, static_methods0 = compile_expr_rec e0 in
      let after_label = label_gen.ctrl_label () in
      let case_exprs = List.map (fun (_, case_expr) -> case_expr) cases in
      let compiled_case_exprs = List.map compile_expr_rec case_exprs in
      let case_defs =
        List.map (fun (defs, _, _) -> defs) compiled_case_exprs |> List.flatten
      in

      let case_code_with_labels =
        List.map (fun (_, code, _) -> code) compiled_case_exprs
        |> List.map (fun code ->
               let label = label_gen.ctrl_label () in
               (label, [ LABEL label ] @ code @ [ GOTO after_label ]))
      in

      let case_code =
        case_code_with_labels
        |> List.map (fun (_, code) -> code)
        |> List.flatten
      in

      let case_smethods =
        List.map (fun (_, _, smethods) -> smethods) compiled_case_exprs
        |> List.flatten
      in

      let index_labels =
        List.map (fun (label, _) -> label) case_code_with_labels
        |> List.combine (List.map (fun (con, _) -> con_index con) cases)
      in

      let index_getter = get_index (get_expr_type e0) in

      match fallback_opt with
      | None ->
          (* because java requires a default case, we must be crafty and replace the last case with default *)
          let last_case_label =
            List.rev index_labels |> List.hd |> fun (_, l) -> l
          in
          let other_index_labels =
            List.rev index_labels |> List.tl |> List.rev
          in
          ( defs0 @ case_defs,
            code0 @ index_getter
            @ [ LOOKUP_SWITCH (other_index_labels, last_case_label) ]
            @ case_code @ [ LABEL after_label ],
            static_methods0 @ case_smethods )
      | Some fallback_expr ->
          let default_defs, default_code, default_static_methods =
            compile_expr_rec fallback_expr
          in
          let default_label = label_gen.ctrl_label () in
          let default_code = [ LABEL default_label ] @ default_code in
          ( defs0 @ case_defs @ default_defs,
            code0 @ index_getter
            @ [ LOOKUP_SWITCH (index_labels, default_label) ]
            @ case_code @ default_code @ [ LABEL after_label ],
            static_methods0 @ case_smethods @ default_static_methods )
      (*
      let (default_label_opt, default_defs, default_code, default_static_methods) =
        fallback_opt
        |> Option.map (fun fallback -> let defs, code, s = compile_expr_rec fallback in
        let label = label_gen.ctrl_label () in
        (Some label, defs, [LABEL label] @ code, s)
        )
        |> Option.value ~default:(None, [], [], [])
      in

    let index_getter = get_index (get_expr_type e0) in
    *)
      )
  | Match_Failure -> ([], [ MATCH_FAILURE ], [])

and compile_bop label_gen env top_level_bindings e0 e1 =
  let compile_expr_rec = compile_expr label_gen env top_level_bindings in
  function
  | (ADD | SUB | MUL | DIV) as int_to_int_op ->
      let defs0, c0, s0 = compile_expr_rec e0 in
      let defs1, c1, s1 = compile_expr_rec e1 in
      let standardise_bop = function
        | Common.ADD -> Instruction.ADD
        | SUB -> SUB
        | MUL -> MUL
        | DIV -> DIV
        | _ -> assert false
      in
      ( defs0 @ defs1,
        c0 @ [ UNBOX_INT ] @ c1 @ [ UNBOX_INT ]
        @ [ BOP (standardise_bop int_to_int_op); BOX_INT ],
        s0 @ s1 )
  | (LT | GT) as int_to_bool_op ->
      let defs0, c0, s0 = compile_expr_rec e0 in
      let defs1, c1, s1 = compile_expr_rec e1 in
      let standard_bop =
        if int_to_bool_op = LT then Instruction.LT else Instruction.GT
      in
      ( defs0 @ defs1,
        c0 @ [ UNBOX_INT ] @ c1 @ [ UNBOX_INT ] @ [ BOP standard_bop; BOX_BOOL ],
        s0 @ s1 )
  | AND ->
      let defs0, c0, s0 = compile_expr_rec e0 in
      let defs1, c1, s1 = compile_expr_rec e1 in
      let false_label = label_gen.ctrl_label () in
      let after_label = label_gen.ctrl_label () in
      ( defs0 @ defs1,
        c0
        @ [ UNBOX_BOOL; IFZERO false_label ]
        @ c1
        @ [ UNBOX_BOOL; IFZERO false_label ]
        @ [
            PUSH_BOOL true;
            BOX_BOOL;
            GOTO after_label;
            LABEL false_label;
            PUSH_BOOL false;
            BOX_BOOL;
            LABEL after_label;
          ],
        s0 @ s1 )
  | OR ->
      let defs0, c0, s0 = compile_expr_rec e0 in
      let defs1, c1, s1 = compile_expr_rec e1 in
      let true_label = label_gen.ctrl_label () in
      let after_label = label_gen.ctrl_label () in
      ( defs0 @ defs1,
        c0
        @ [ UNBOX_BOOL; IFNONZERO true_label ]
        @ c1
        @ [ UNBOX_BOOL; IFNONZERO true_label ]
        @ [
            PUSH_BOOL false;
            BOX_BOOL;
            GOTO after_label;
            LABEL true_label;
            PUSH_BOOL true;
            BOX_BOOL;
            LABEL after_label;
          ],
        s0 @ s1 )
  (* polymorphic eq *)
  | EQ ->
      let defs0, c0, smethods0 = compile_expr_rec e0 in
      let defs1, c1, smethods1 = compile_expr_rec e1 in
      (defs0 @ defs1, c0 @ c1 @ [ BOP EQ; BOX_BOOL ], smethods0 @ smethods1)

and compile_anon_lambda_expr label_gen env top_level_bindings
    (arg_type, return_type, x, e) =
  let fvars_with_types =
    free_vars_with_types_expr (StringSet.add x top_level_bindings) e
    |> StringMap.to_seq |> List.of_seq
    |> List.map (fun (x, ty) -> (x, convert_type ty))
  in

  let body_label_gen = reset_per_func_generators label_gen in

  let body_env =
    Value_env.strip_nonstatic env
    |> Value_env.add_local_var x (body_label_gen.ref_label ())
  in
  let body_env =
    List.fold_left
      (fun acc (name, ty) -> Value_env.add_instance_field name (name, ty) acc)
      body_env fvars_with_types
  in
  let defs, ecode, smethods =
    compile_expr body_label_gen body_env top_level_bindings e
  in
  compile_anon_lambda label_gen env fvars_with_types defs
    (arg_type, return_type, ecode)
    smethods

and compile_anon_lambda label_gen env fvars_with_types defs
    (arg_type, return_type, body_code) smethods =
  let fvars = List.map (fun (x, _) -> x) fvars_with_types in
  let fvar_types = List.map (fun (_, y) -> y) fvars_with_types in
  let fetch_fvars = List.map (fun x -> Value_env.lookup x env) fvars in

  let closure_label = label_gen.lambda_label () in

  let closure =
    Closure
      {
        name = closure_label;
        constructor_args = fvars_with_types;
        arg_type;
        return_type;
        body = body_code;
      }
  in
  ( defs @ [ closure ],
    [ ALLOC_OBJ closure_label ]
    @ (List.rev fetch_fvars |> List.flatten)
    @ [ CONSTRUCT_OBJ (closure_label, List.rev fvar_types) ],
    smethods )

and compile_dyn_lambda_expr label_gen env top_level_bindings
    (arg_type, return_type, x, e) =
  let fvars_with_types =
    free_vars_with_types_expr (StringSet.add x top_level_bindings) e
    |> StringMap.to_seq |> List.of_seq
    |> List.map (fun (x, ty) -> (x, convert_type ty))
  in

  let body_label_gen = reset_for_static_func_generators label_gen in

  let body_env =
    Value_env.strip_nonstatic env |> fun env ->
    List.fold_left
      (fun env (fv, _) ->
        Value_env.add_local_var fv (body_label_gen.ref_label ()) env)
      env fvars_with_types
    |> Value_env.add_local_var x (body_label_gen.ref_label ())
  in

  let defs, ecode, smethods =
    compile_expr body_label_gen body_env top_level_bindings e
  in
  compile_dyn_lambda label_gen env fvars_with_types defs
    (arg_type, return_type, ecode)
    smethods

and compile_dyn_lambda label_gen env fvars_with_types defs
    (arg_type, return_type, body_code) smethods =
  let fvars = List.map (fun (x, _) -> x) fvars_with_types in
  let fvar_types = List.map (fun (_, y) -> y) fvars_with_types in
  let fetch_fvars = List.map (fun x -> Value_env.lookup x env) fvars in

  let lifted_label = label_gen.lambda_label () in

  let lifted_method =
    {
      name = lifted_label;
      args = fvar_types @ [ arg_type ];
      return_type;
      body = body_code;
    }
  in
  ( defs,
    (List.rev fetch_fvars |> List.flatten)
    @ [
        CREATE_DYNAMIC_CLOSURE (lifted_label, fvar_types, arg_type, return_type);
      ],
    smethods @ [ lifted_method ] )

and lower_tuple_eles_to_array label_gen env top_level_bindings es =
  let prelude = [ PUSH_INT (List.length es); ALLOC_ARRAY "java/lang/Object" ] in
  let main_col =
    List.mapi
      (fun i e ->
        let edefs, ecode, smethods =
          compile_expr label_gen env top_level_bindings e
        in
        (edefs, [ DUP; PUSH_INT i ] @ ecode @ [ STORE_ARRAY ], smethods))
      es
  in
  let defs, maincode, smethods =
    List.fold_right
      (fun (defs, code, smethods) (accdefs, acccode, accsmethods) ->
        (defs @ accdefs, code @ acccode, smethods @ accsmethods))
      main_col ([], [], [])
  in
  (defs, prelude @ maincode, smethods)

let lower_type_constructor tname
    (Desugared_ast.DeclConstr (cname, tag, type_expr_opt)) =
  Constructor
    { name = cname; tag; tname; arg = Option.map convert_type type_expr_opt }

let lambda_for_constructor label_gen env cname typedef_texpr arg =
  match arg with
  | None ->
      ( [],
        [
          ALLOC_OBJ cname;
          CONSTRUCT_OBJ (cname, []);
          STORE_STATIC ("Foo", cname, typedef_texpr);
        ],
        [],
        Value_env.add_static_field cname ("Foo", cname, typedef_texpr) env )
  | Some ty ->
      let body =
        [ ALLOC_OBJ cname; LOAD_REF 1; CONSTRUCT_OBJ (cname, [ ty ]) ]
      in
      let fvars_with_types = [] in

      let defs, code, smethods =
        compile_anon_lambda label_gen env fvars_with_types []
          (ty, typedef_texpr, body) []
      in
      ( defs,
        code @ [ STORE_STATIC ("Foo", cname, TyFun (ty, typedef_texpr)) ],
        smethods,
        Value_env.add_static_field cname
          ("Foo", cname, TyFun (ty, typedef_texpr))
          env )

let compile_decl label_gen env toplevel = function
  | Desugared_ast.Val (ty, x, e) ->
      let defs, c, smethods = compile_expr label_gen env toplevel e in
      let x_label = label_gen.static_label () in
      let env' =
        Value_env.add_static_field x ("Foo", x_label, convert_type ty) env
      in
      let new_toplevel = StringSet.add x toplevel in
      ( defs,
        c @ [ STORE_STATIC ("Foo", x_label, convert_type ty) ],
        smethods,
        env',
        new_toplevel )
  | Desugared_ast.ValRec (ty, x, e) ->
      (* make value available in e for compiling recursion *)
      let x_label = label_gen.static_label () in
      let env' =
        Value_env.add_static_field x ("Foo", x_label, convert_type ty) env
      in
      let new_toplevel = StringSet.add x toplevel in
      let defs, c, smethods = compile_expr label_gen env' new_toplevel e in
      ( defs,
        c @ [ STORE_STATIC ("Foo", x_label, convert_type ty) ],
        smethods,
        env',
        new_toplevel )
  | Desugared_ast.Type (ty, _, tname, type_constructors) ->
      let class_tname = String.capitalize_ascii tname in
      let typei =
        Type_interface
          {
            name = class_tname;
            constructors =
              List.map
                (function Desugared_ast.DeclConstr (cname, _, _) -> cname)
                type_constructors;
          }
      in
      let tconstrs =
        List.map (lower_type_constructor class_tname) type_constructors
      in
      (* current bodge - adding static fields for creating constructors *)
      let defs, code, smethods, env =
        List.fold_right
          (fun (Desugared_ast.DeclConstr (cname, _, type_expr_op))
               (accdefs, acccode, accsmethods, env) ->
            let defs, code, smethods, env =
              lambda_for_constructor label_gen env cname (convert_type ty)
                (Option.map convert_type type_expr_op)
            in
            (defs @ accdefs, code @ acccode, smethods @ accsmethods, env))
          type_constructors ([], [], [], env)
      in
      ([ typei ] @ tconstrs @ defs, code, smethods, env, toplevel)

let compile_decl_from_scratch d =
  let generators = make_generators () in
  let defs, code, smethods, _, _ =
    compile_decl generators Value_env.stdlib StringSet.empty d
  in
  { declarations = defs; code; static_methods = smethods }

let compile_program_from_scratch p =
  let generators = make_generators () in
  List.fold_left
    (fun (defsacc, codeacc, smethodsacc, envacc, tlacc) d ->
      let defs, code, smethods, env, tl =
        compile_decl generators envacc tlacc d
      in
      (defsacc @ defs, codeacc @ code, smethodsacc @ smethods, env, tl))
    ([], [], [], Value_env.stdlib, StringSet.empty)
    p
  |> fun (defs, code, smethods, _, _) ->
  { declarations = defs; code; static_methods = smethods }
