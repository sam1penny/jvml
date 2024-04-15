open Linear.Instruction
open Printf

let stack_size_change = function
  | PUSH_INT _ | PUSH_FLOAT _ | PUSH_BOOL _ | PUSH_UNIT | PUSH_STRING _ -> 1
  | BOX_INT | BOX_FLOAT | BOX_BOOL | UNBOX_INT | UNBOX_BOOL | UNBOX_FLOAT -> 0
  | STORE_REF _ -> -1
  | LOAD_REF _ -> 1
  | IFZERO _ | IFNONZERO _ -> -1
  | GOTO _ | LABEL _ -> 0
  | BOP _ -> 1
  | LOAD_FIELD _ | LOAD_FIELD_ANY_CLASS _ -> 0
  | STORE_FIELD _ | STORE_FIELD_ANY_CLASS _ -> -2
  | ALLOC_OBJ _ -> 2
  | CONSTRUCT_OBJ (_, tys) -> -1 + List.length tys
  | ALLOC_ARRAY _ -> 0
  | STORE_ARRAY -> -3
  | DUP -> 1
  | POP -> -1
  | APPLY _ -> -1
  | LOAD_STATIC _ -> 1
  | STORE_STATIC _ -> -1
  | CREATE_DYNAMIC_CLOSURE (_, tys, _, _) -> 1 - List.length tys
  (* todo - these might have to be increased, as they are transformed
     into multiple bytecode instructions that, during execution,
     may push multiple values onto the stack.
  *)
  | TUPLE_GET _ -> 2
  | CONSTRUCTOR_GET _ -> 0
  | SWITCH _ -> -1
  | MATCH_FAILURE -> 2
  | CONSTRUCTOR_INDEX _ -> 0
  | STATIC_APPLY (_, ty_args, _, _) -> 1 - List.length ty_args
  | NULL -> 1

let max_stack_depth prog =
  List.map stack_size_change prog
  |> List.fold_left_map (fun acc x -> (acc + x, acc + x)) 0
  |> fun (_, l) -> List.fold_left max 0 l

let num_local_vars nargs prog =
  List.map (function LOAD_REF n | STORE_REF n -> n | _ -> 0) prog
  |> List.fold_left max nargs |> ( + ) 1

let rec lower_type = function
  | TyInt -> "java/lang/Integer"
  | TyFloat -> "java/lang/Float"
  | TyString -> "java/lang/String"
  | TyBool -> "java/lang/Boolean"
  | TyFun _ -> "java/util/function/Function"
  | TyAny -> "java/lang/Object"
  | TyUnit -> "sam/generated/Unit"
  | TyCustom c -> Printf.sprintf "sam/generated/%s" (String.capitalize_ascii c)
  | TyTuple _ -> "sam/generated/Tuple"
  | TyArray t -> lower_type t

let rec lower_type_as_descriptor ty =
  match ty with
  | TyInt | TyFloat | TyString | TyBool | TyFun _ | TyAny | TyUnit | TyCustom _
  | TyTuple _ ->
      "L" ^ lower_type ty ^ ";"
  | TyArray t -> "[" ^ lower_type_as_descriptor t

let lower_type_list tys =
  List.map lower_type_as_descriptor tys |> String.concat ""

let make_jvm_ctrl_gen () =
  let cnt = Linear.Lower.make_counter 0 in
  fun () -> "Ljvm" ^ string_of_int @@ cnt ()

let lower_bop ctrl_gen = function
  | Linear.Instruction.ADD -> [ "iadd" ]
  | SUB -> [ "isub" ]
  | MUL -> [ "imul" ]
  | DIV -> [ "idiv" ]
  | FLOAT_ADD -> [ "fadd" ]
  | FLOAT_SUB -> [ "fsub" ]
  | FLOAT_MUL -> [ "fmul" ]
  | FLOAT_DIV -> [ "fdiv" ]
  | EQ ->
      [ "invokevirtual Method java/lang/Object equals (Ljava/lang/Object;)Z" ]
  | (LT | GT) as bop ->
      let false_label = ctrl_gen () in
      let after_label = ctrl_gen () in
      let comparison = if bop = LT then "if_icmpge" else "if_icmple" in
      [
        comparison ^ " " ^ false_label;
        "iconst_1";
        "goto " ^ after_label;
        false_label ^ ":";
        "iconst_0";
        after_label ^ ":";
      ]
  | STRING_CONCAT ->
      [
        "invokevirtual Method java/lang/String concat \
         (Ljava/lang/String;)Ljava/lang/String;";
      ]

let load_int = function
  | -1l -> "iconst_m1"
  | (0l | 1l | 2l | 3l | 4l | 5l) as n -> sprintf "iconst_%ld" n
  | n -> sprintf "ldc %ld" n

let load_float = function
  | 0.0 -> "fconst_0"
  | 1.0 -> "fconst_1"
  | 2.0 -> "fconst_2"
  | f -> sprintf "ldc %ff" f

let store_ref = function
  | (0 | 1 | 2 | 3) as n -> sprintf "astore_%i" n
  | n -> sprintf "astore %i" n

let load_ref = function
  | (0 | 1 | 2 | 3) as n -> sprintf "aload_%i" n
  | n -> sprintf "aload %i" n

let lower_dyn_closure lifted captured_tys arg_type return_type =
  [
    sprintf
      "invokedynamic InvokeDynamic invokeStatic Method \
       java/lang/invoke/LambdaMetafactory metafactory \
       (Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite; \
       MethodType (Ljava/lang/Object;)Ljava/lang/Object; MethodHandle \
       invokeStatic Method sam/generated/Foo %s (%s)L%s; MethodType (L%s;)L%s; \
       : apply (%s)Ljava/util/function/Function;"
      lifted
      (lower_type_list (captured_tys @ [ arg_type ]))
      (lower_type return_type) (lower_type arg_type) (lower_type return_type)
      (lower_type_list captured_tys);
  ]

let lower_switch switch_type cases default_lab =
  match switch_type with
  | LOOKUP ->
      [ "lookupswitch" ]
      @ List.map (fun (index, lab) -> sprintf "%ld : %s" index lab) cases
      @ [ sprintf "default : %s" default_lab ]
  | TABLE i ->
      [ sprintf "tableswitch %i" i ]
      @ List.map (fun (_, lab) -> lab) cases
      @ [ sprintf "default : %s" default_lab ]

let lower_instruction ctrl_gen clazz = function
  | PUSH_INT i -> [ load_int i ]
  | BOX_INT ->
      [ "invokestatic Method java/lang/Integer valueOf (I)Ljava/lang/Integer;" ]
  | UNBOX_INT -> [ "invokevirtual Method java/lang/Integer intValue ()I" ]
  | PUSH_FLOAT f -> [ load_float f ]
  | BOX_FLOAT ->
      [ "invokestatic Method java/lang/Float valueOf (F)Ljava/lang/Float;" ]
  | UNBOX_FLOAT -> [ "invokevirtual Method java/lang/Float floatValue ()F" ]
  | PUSH_BOOL b -> [ (if b then "iconst_1" else "iconst_0") ]
  | BOX_BOOL ->
      [ "invokestatic Method java/lang/Boolean valueOf (Z)Ljava/lang/Boolean;" ]
  | UNBOX_BOOL -> [ "invokevirtual Method java/lang/Boolean booleanValue ()Z" ]
  | PUSH_UNIT ->
      [ "getstatic Field sam/generated/Unit INSTANCE Lsam/generated/Unit;" ]
  | PUSH_STRING s -> [ sprintf "ldc \"%s\"" s ]
  | BOP bop -> lower_bop ctrl_gen bop
  | STORE_REF r -> [ store_ref r ]
  | LOAD_REF r -> [ load_ref r ]
  | IFZERO l -> [ sprintf "ifeq %s" l ]
  | IFNONZERO l -> [ sprintf "ifne %s" l ]
  | GOTO l -> [ sprintf "goto %s" l ]
  | LABEL l -> [ sprintf "%s:" l ]
  | LOAD_FIELD (f, ty) ->
      [
        sprintf "getfield Field sam/generated/%s %s %s" clazz f
          (lower_type_as_descriptor ty);
      ]
  | LOAD_FIELD_ANY_CLASS (clazz, f, ty) ->
      [
        sprintf "getfield Field sam/generated/%s %s %s" clazz f
          (lower_type_as_descriptor ty);
      ]
  | STORE_FIELD (f, ty) ->
      [
        sprintf "putfield Field sam/generated/%s %s %s" clazz f
          (lower_type_as_descriptor ty);
      ]
  | STORE_FIELD_ANY_CLASS (clazz, f, ty) ->
      [
        sprintf "putfield Field sam/generated/%s %s %s" clazz f
          (lower_type_as_descriptor ty);
      ]
  | ALLOC_OBJ name -> [ sprintf "new sam/generated/%s" name; "dup" ]
  | CONSTRUCT_OBJ (name, tys) ->
      [
        sprintf "invokespecial Method sam/generated/%s <init> (%s)V" name
          (lower_type_list tys);
      ]
  | ALLOC_ARRAY name -> [ sprintf "anewarray %s" name ]
  | STORE_ARRAY -> [ "aastore" ]
  | DUP -> [ "dup" ]
  | POP -> [ "pop" ]
  | APPLY ty ->
      [
        "invokeinterface InterfaceMethod java/util/function/Function apply \
         (Ljava/lang/Object;)Ljava/lang/Object; 2";
        sprintf "checkcast %s" (lower_type ty);
      ]
  | LOAD_STATIC (clazz, f, ty) ->
      [
        sprintf "getstatic Field sam/generated/%s %s L%s;" clazz f
          (lower_type ty);
      ]
  | STORE_STATIC (clazz, f, ty) ->
      [
        sprintf "putstatic Field sam/generated/%s %s L%s;" clazz f
          (lower_type ty);
      ]
  | CREATE_DYNAMIC_CLOSURE (lifted, captured_tys, arg_type, return_type) ->
      lower_dyn_closure lifted captured_tys arg_type return_type
  | TUPLE_GET (ty, i) ->
      [
        "getfield Field sam/generated/Tuple data [Ljava/lang/Object;";
        load_int i;
        "aaload";
        sprintf "checkcast %s" (lower_type ty);
      ]
  | CONSTRUCTOR_GET (ty, cname) ->
      (* todo - cast once on switch case entry rather than on each get *)
      [
        sprintf "checkcast sam/generated/%s" cname;
        sprintf "getfield Field sam/generated/%s val L%s;" cname (lower_type ty);
      ]
  | SWITCH (switch_type, cases, default_lab) ->
      lower_switch switch_type cases default_lab
  | CONSTRUCTOR_INDEX tname ->
      [
        sprintf "getfield Field sam/generated/%s tag I"
          (String.capitalize_ascii tname);
      ]
  | MATCH_FAILURE ->
      [
        "new sam/generated/MatchFailure";
        "dup";
        "invokespecial Method sam/generated/MatchFailure <init> ()V";
        "athrow";
      ]
  | STATIC_APPLY (name, arg_tys, ret_ty, actual_return_ty) ->
      [
        sprintf "invokestatic Method sam/generated/Foo %s (%s)L%s;" name
          (lower_type_list arg_tys) (lower_type ret_ty);
        sprintf "checkcast %s" (lower_type actual_return_ty);
      ]
  | NULL -> [ "aconst_null" ]

let should_indent = function LABEL _ -> false | _ -> true

let lower_body indent clazz b =
  let ctrl_gen = make_jvm_ctrl_gen () in
  List.map (lower_instruction ctrl_gen clazz) b
  |> List.flatten
  (* slightly dodgy bodge to only apply indent to labels *)
  |> List.map (fun i ->
         if String.starts_with ~prefix:"L" i && String.ends_with ~suffix:":" i
         then i
         else indent ^ i)
  |> String.concat "\n"

let lower_constructor_args args =
  List.map
    (* todo - consider making private (then requires invokespecial) *)
      (fun (name, ty) -> sprintf ".field public %s L%s;" name (lower_type ty))
    args
  |> String.concat "\n"

let lower_constructor_body indent name constructor_args =
  List.mapi
    (fun i (field, ty) ->
      [
        "aload_0";
        load_ref (i + 1);
        sprintf "putfield Field sam/generated/%s %s L%s;" name field
          (lower_type ty);
      ]
      |> List.map (fun i -> indent ^ i)
      |> String.concat "\n")
    constructor_args
  |> String.concat "\n"

let lower_closure (c : closure) =
  let indent = "    " in
  sprintf
    {|
.class public sam/generated/%s
.super java/lang/Object
.implements java/util/function/Function
%s

.method <init> : (%s)V
  .code stack 2 locals %i
    aload_0
    invokespecial Method java/lang/Object <init> ()V
%s
    return
  .end code
.end method

.method public apply : (Ljava/lang/Object;)Ljava/lang/Object;
  .code stack %i locals %i
    aload_1
    checkcast %s
    astore_1
%s
    areturn
  .end code
.end method

.method public equals : (Ljava/lang/Object;)Z
    .code stack 3 locals 2
L0:     new java/lang/IllegalStateException
L3:     dup
L4:     ldc "should not be able to call equality on lambda function"
L6:     invokespecial Method java/lang/IllegalStateException <init> (Ljava/lang/String;)V
L9:     athrow

    .end code
.end method

.end class
    |}
    c.name
    (lower_constructor_args c.constructor_args)
    (List.map (fun (_, ty) -> ty) c.constructor_args |> lower_type_list)
    (1 + List.length c.constructor_args)
    (lower_constructor_body indent c.name c.constructor_args)
    (max_stack_depth c.body) (num_local_vars 1 c.body) (lower_type c.arg_type)
    (lower_body "    " c.name c.body)

let lower_type_interface (ti : type_interface) =
  sprintf
    {|
.version 62 0
.class public super abstract sam/generated/%s
.super java/lang/Object
.field tag I
.permittedsubclasses %s

.method public <init> : ()V
    .code stack 1 locals 1
L0:     aload_0
L1:     invokespecial Method java/lang/Object <init> ()V
L4:     return
    .end code
.end method

.end class
|}
    ti.name
    (List.map (fun c -> "sam/generated/" ^ c) ti.constructors
    |> String.concat " ")

let lower_value_constructor (vc : constructor) =
  sprintf
    {|
.version 62 0
.class public final super sam/generated/%s
.super sam/generated/%s
%s

.method public <init> : (%s)V
    .code stack 2 locals 3
L0:     aload_0
L1:     invokespecial Method sam/generated/%s <init> ()V
%s
%s
L14:    return

    .end code
.end method

.method public toString : ()Ljava/lang/String;
    .code stack 3 locals 1
L0:     new java/lang/StringBuilder
L3:     dup
L4:     ldc "%s"
L6:     invokespecial Method java/lang/StringBuilder <init> (Ljava/lang/String;)V
%s
L16:    invokevirtual Method java/lang/StringBuilder toString ()Ljava/lang/String;
L19:    areturn

    .end code
.end method

.method public equals : (Ljava/lang/Object;)Z
    .code stack 2 locals 2
L0:     aload_0
L1:     aload_1
L2:     if_acmpne L7
L5:     iconst_1
L6:     ireturn

        .stack same
L7:     aload_0
L8:     invokevirtual Method java/lang/Object getClass ()Ljava/lang/Class;
L11:    aload_1
L12:    invokevirtual Method java/lang/Object getClass ()Ljava/lang/Class;
L15:    if_acmpeq L20
L18:    iconst_0
L19:    ireturn

        .stack same
%s
L34:    ireturn
    .end code
.end method

.end class
|}
    vc.name vc.tname
    (Option.map
       (fun arg -> sprintf ".field public val L%s;" (lower_type arg))
       vc.arg
    |> Option.value ~default:"")
    (Option.map (fun arg -> "L" ^ lower_type arg ^ ";") vc.arg
    |> Option.value ~default:"")
    vc.tname
    ([
       "aload_0";
       load_int vc.tag;
       sprintf "putfield Field sam/generated/%s tag I" vc.name;
     ]
    |> String.concat "\n")
    (Option.map
       (fun arg ->
         [
           "aload_0";
           "aload_1";
           sprintf "putfield Field sam/generated/%s val L%s;" vc.name
             (lower_type arg);
         ]
         |> String.concat "\n")
       vc.arg
    |> Option.value ~default:"")
    vc.name
    (Option.map
       (fun arg ->
         let call_stringbuilder_append =
           "invokevirtual Method java/lang/StringBuilder append \
            (Ljava/lang/Object;)Ljava/lang/StringBuilder;"
         in
         [
           {|ldc " ("|};
           call_stringbuilder_append;
           "aload_0";
           sprintf "getfield Field sam/generated/%s val L%s;" vc.name
             (lower_type arg);
           call_stringbuilder_append;
           {|ldc ")"|};
           call_stringbuilder_append;
         ]
         |> String.concat "\n")
       vc.arg
    |> Option.value ~default:"")
    (Option.map
       (fun arg ->
         sprintf
           {|
L20:    aload_0
L21:    getfield Field sam/generated/%s val L%s;
L24:    aload_1
L25:    checkcast sam/generated/%s
L28:    getfield Field sam/generated/%s val L%s;
L31:    invokevirtual Method java/lang/Object equals (Ljava/lang/Object;)Z
      |}
           vc.name (lower_type arg) vc.name vc.name (lower_type arg))
       vc.arg
    |> Option.value ~default:{|
L20:    iconst_1
    |})

let lower_declaration = function
  | Closure c -> lower_closure c
  | Type_interface ti -> lower_type_interface ti
  | Constructor c -> lower_value_constructor c

let lower_static_method static_method =
  sprintf
    {|
.method public static synthetic %s : (%s)L%s;
  .code stack %i locals %i
%s
    areturn

  .end code
.end method
  |}
    static_method.name
    (lower_type_list static_method.args)
    (lower_type static_method.return_type)
    (max_stack_depth static_method.body)
    (num_local_vars (List.length static_method.args) static_method.body)
    (lower_body "     " "sam/generated/Foo" static_method.body)

let lower_field_defs p =
  List.filter_map
    (function STORE_STATIC (_, f, ty) -> Some (f, ty) | _ -> None)
    p
  |> List.map (fun (f, ty) ->
         sprintf ".field public static %s L%s;" f (lower_type ty))
  |> String.concat "\n"

let produce_instruction_bytecode (p, static_methods) =
  (* .version > 50 required for invokedynamic, however requires stackmaptable *)
  sprintf
    {|
.version 52 0
.class public sam/generated/Foo
.super java/lang/Object
%s
.method public static main : ([Ljava/lang/String;)V
    .code stack %i locals %i
%s
        return
  .end code
.end method
%s
.bootstrapmethods
.innerclasses
    java/lang/invoke/MethodHandles$Lookup java/lang/invoke/MethodHandles Lookup public static final
.end innerclasses
.end class
|}
    (lower_field_defs p) (max_stack_depth p) (num_local_vars 1 p)
    (lower_body "        " "sam/generated/Foo" p)
    (List.map lower_static_method static_methods |> String.concat "\n")

let external_lib =
  {|
.version 62 0
.class public super sam/generated/MatchFailure
.super java/lang/RuntimeException

.method public <init> : ()V
    .code stack 1 locals 1
        aload_0
        invokespecial Method java/lang/RuntimeException <init> ()V
        return
    .end code
.end method
.end class

; my custom tuple class
.version 62 0
.class public super sam/generated/Tuple
.super java/lang/Object
.field public data [Ljava/lang/Object;

.method <init> : ([Ljava/lang/Object;)V
    .code stack 2 locals 2
L0:     aload_0
L1:     invokespecial Method java/lang/Object <init> ()V
L4:     aload_0
L5:     aload_1
L6:     putfield Field sam/generated/Tuple data [Ljava/lang/Object;
L9:     return

    .end code
.end method

.method public toString : ()Ljava/lang/String;
    .code stack 1 locals 1
L0:     aload_0
L1:     getfield Field sam/generated/Tuple data [Ljava/lang/Object;
L4:     invokestatic Method java/util/Arrays toString ([Ljava/lang/Object;)Ljava/lang/String;
L7:     areturn

    .end code
.end method

.method public equals : (Ljava/lang/Object;)Z
    .code stack 2 locals 2
L0:     aload_0
L1:     getfield Field sam/generated/Tuple data [Ljava/lang/Object;
L4:     aload_1
L5:     checkcast sam/generated/Tuple
L8:     getfield Field sam/generated/Tuple data [Ljava/lang/Object;
L11:    invokestatic Method java/util/Arrays equals ([Ljava/lang/Object;[Ljava/lang/Object;)Z
L14:    ireturn

    .end code
.end method

.method public hashCode : ()I
    .code stack 1 locals 1
L0:     aload_0
L1:     getfield Field sam/generated/Tuple data [Ljava/lang/Object;
L4:     invokestatic Method java/util/Arrays hashCode ([Ljava/lang/Object;)I
L7:     ireturn

    .end code
.end method
.sourcefile "Tuple.java"
.end class

.class public super sam/generated/Unit
.super java/lang/Object
.field public static INSTANCE Lsam/generated/Unit;

.method private <init> : ()V
    .code stack 1 locals 1
      aload_0
      invokespecial Method java/lang/Object <init> ()V
      return
    .end code
.end method

.method public toString : ()Ljava/lang/String;
    .code stack 1 locals 1
L0:     ldc "()"
L2:     areturn

    .end code
.end method

.method static <clinit> : ()V
    .code stack 2 locals 0
      new sam/generated/Unit
      dup
      invokespecial Method sam/generated/Unit <init> ()V
      putstatic Field sam/generated/Unit INSTANCE Lsam/generated/Unit;
      return
    .end code
.end method
.end class

; my custom list class

.version 65 0
.class public super abstract sam/generated/List
.super java/lang/Object
.field tag I

.method public <init> : ()V
    .code stack 1 locals 1
L0:     aload_0
L1:     invokespecial Method java/lang/Object <init> ()V
L4:     return

    .end code
.end method
.permittedsubclasses sam/generated/Cons$ sam/generated/Nil$
.end class


.version 65 0
.class public final super sam/generated/Nil$
.super sam/generated/List

.method public <init> : ()V
    .code stack 2 locals 1
L0:     aload_0
L1:     invokespecial Method sam/generated/List <init> ()V
        aload_0
        iconst_0
        putfield Field sam/generated/Nil$ tag I
L4:     return

    .end code
.end method

.method public toString : ()Ljava/lang/String;
    .code stack 1 locals 1
L0:     ldc "[]"
L2:     areturn

    .end code
.end method
.end class


.version 65 0
.class public final super sam/generated/Cons$
.super sam/generated/List
.field public val Lsam/generated/Tuple;

.method public <init> : (Lsam/generated/Tuple;)V
    .code stack 2 locals 2
L0:     aload_0
L1:     invokespecial Method sam/generated/List <init> ()V
L4:     aload_0
L5:     aload_1
L6:     putfield Field sam/generated/Cons$ val Lsam/generated/Tuple;
        aload_0
        iconst_1
        putfield Field sam/generated/Cons$ tag I
L9:     return

    .end code
.end method

.method public toString : ()Ljava/lang/String;
    .code stack 3 locals 3
L0:     new java/lang/StringBuilder
L3:     dup
L4:     ldc "["
L6:     invokespecial Method java/lang/StringBuilder <init> (Ljava/lang/String;)V
L9:     astore_1
L10:    aload_0
L11:    astore_2

        .stack append Object java/lang/StringBuilder Object sam/generated/Cons$
L12:    aload_2
L13:    getfield Field sam/generated/Cons$ val Lsam/generated/Tuple;
L16:    getfield Field sam/generated/Tuple data [Ljava/lang/Object;
L19:    iconst_1
L20:    aaload
L21:    invokevirtual Method java/lang/Object getClass ()Ljava/lang/Class;
L24:    ldc Class sam/generated/Nil$
L26:    if_acmpeq L64
L29:    aload_1
L30:    aload_2
L31:    getfield Field sam/generated/Cons$ val Lsam/generated/Tuple;
L34:    getfield Field sam/generated/Tuple data [Ljava/lang/Object;
L37:    iconst_0
L38:    aaload
L39:    invokevirtual Method java/lang/StringBuilder append (Ljava/lang/Object;)Ljava/lang/StringBuilder;
L42:    ldc ";"
L44:    invokevirtual Method java/lang/StringBuilder append (Ljava/lang/String;)Ljava/lang/StringBuilder;
L47:    pop
L48:    aload_2
L49:    getfield Field sam/generated/Cons$ val Lsam/generated/Tuple;
L52:    getfield Field sam/generated/Tuple data [Ljava/lang/Object;
L55:    iconst_1
L56:    aaload
L57:    checkcast sam/generated/Cons$
L60:    astore_2
L61:    goto L12

        .stack same
L64:    aload_1
L65:    aload_2
L66:    getfield Field sam/generated/Cons$ val Lsam/generated/Tuple;
L69:    getfield Field sam/generated/Tuple data [Ljava/lang/Object;
L72:    iconst_0
L73:    aaload
L74:    invokevirtual Method java/lang/StringBuilder append (Ljava/lang/Object;)Ljava/lang/StringBuilder;
L77:    pop
L78:    aload_1
L79:    ldc "]"
L81:    invokevirtual Method java/lang/StringBuilder append (Ljava/lang/String;)Ljava/lang/StringBuilder;
L84:    invokevirtual Method java/lang/StringBuilder toString ()Ljava/lang/String;
L87:    areturn

    .end code
.end method
.end class

.version 62 0
.class public super sam/generated/Std
.super java/lang/Object
.field public static print Ljava/util/function/Function; .fieldattributes
    .signature Ljava/util/function/Function<Ljava/lang/Object;Lsam/generated/Unit;>;
.end fieldattributes
.field public static Nil$ Lsam/generated/List;
.field public static Cons$ Ljava/util/function/Function;

.method public <init> : ()V
    .code stack 1 locals 1
L0:     aload_0
L1:     invokespecial Method java/lang/Object <init> ()V
L4:     return
L5:
    .end code
.end method

.method private static synthetic lambda$static$0 : (Ljava/lang/Object;)Lsam/generated/Unit;
    .code stack 2 locals 1
L0:     getstatic Field java/lang/System out Ljava/io/PrintStream;
L3:     aload_0
L4:     invokevirtual Method java/io/PrintStream println (Ljava/lang/Object;)V
L7:     getstatic Field sam/generated/Unit INSTANCE Lsam/generated/Unit;
L10:    areturn
L11:
        .linenumbertable
            L0 5
            L7 6
        .end linenumbertable
        .localvariabletable
            0 is x Ljava/lang/Object; from L0 to L11
        .end localvariabletable
    .end code
.end method

.method private static synthetic lambda$static$1 : (Lsam/generated/Tuple;)Lsam/generated/Cons$;
    .code stack 3 locals 1
        new sam/generated/Cons$
        dup
        aload_0
        invokespecial Method sam/generated/Cons$ <init> (Lsam/generated/Tuple;)V
        areturn

    .end code
.end method


.method static <clinit> : ()V
    .code stack 2 locals 0
L0:     invokedynamic [_25]
L5:     putstatic Field sam/generated/Std print Ljava/util/function/Function;

        new sam/generated/Nil$
        dup
        invokespecial Method sam/generated/Nil$ <init> ()V
        putstatic Field sam/generated/Std Nil$ Lsam/generated/List;

        invokedynamic [_100]
        putstatic Field sam/generated/Std Cons$ Ljava/util/function/Function;

        return
L9:
        .linenumbertable
            L0 4
        .end linenumbertable
    .end code
.end method
.sourcefile "Std.java"
.bootstrapmethods
.innerclasses
    java/lang/invoke/MethodHandles$Lookup java/lang/invoke/MethodHandles Lookup public static final
.end innerclasses
.const [_25] = InvokeDynamic invokeStatic Method java/lang/invoke/LambdaMetafactory metafactory (Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite; MethodType (Ljava/lang/Object;)Ljava/lang/Object; [_59] MethodType (Ljava/lang/Object;)Lsam/generated/Unit; : apply ()Ljava/util/function/Function;
.const [_59] = MethodHandle invokeStatic Method sam/generated/Std lambda$static$0 (Ljava/lang/Object;)Lsam/generated/Unit;
.const [_100] = InvokeDynamic invokeStatic Method java/lang/invoke/LambdaMetafactory metafactory (Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite; MethodType (Ljava/lang/Object;)Ljava/lang/Object; [_101] MethodType (Lsam/generated/Tuple;)Lsam/generated/Cons$; : apply ()Ljava/util/function/Function;
.const [_101] = MethodHandle invokeStatic Method sam/generated/Std lambda$static$1 (Lsam/generated/Tuple;)Lsam/generated/Cons$;
.end class
|}
