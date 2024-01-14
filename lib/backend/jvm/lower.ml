open Linear.Instruction
open Printf

let rec lower_type = function
  | TyInt -> "java/lang/Integer"
  | TyBool -> "java/lang/Boolean"
  | TyFun _ -> "java/util/function/Function"
  | TyAny -> "java/lang/Object"
  | TyUnit -> "Unit"
  | TyCustom c -> String.capitalize_ascii c
  | TyTuple _ -> "Tuple"
  | TyArray t -> lower_type t

let rec lower_type_as_descriptor ty =
  match ty with
  | TyInt | TyBool | TyFun _ | TyAny | TyUnit | TyCustom _ | TyTuple _ ->
      "L" ^ lower_type ty ^ ";"
  | TyArray t -> "[" ^ lower_type_as_descriptor t

let lower_type_list tys =
  List.map lower_type_as_descriptor tys |> String.concat ""

let make_jvm_ctrl_gen () =
  let cnt = Linear.Lower.make_counter () in
  fun () -> "Ljvm" ^ cnt ()

let lower_bop ctrl_gen = function
  | Linear.Instruction.ADD -> [ "iadd" ]
  | SUB -> [ "isub" ]
  | MUL -> [ "imul" ]
  | DIV -> [ "idiv" ]
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

let lower_instruction ctrl_gen clazz = function
  | PUSH_INT i -> [ sprintf "ldc %s" (string_of_int i) ]
  | BOX_INT ->
      [ "invokestatic Method java/lang/Integer valueOf (I)Ljava/lang/Integer;" ]
  | UNBOX_INT -> [ "invokevirtual Method java/lang/Integer intValue ()I" ]
  | PUSH_BOOL b -> [ (if b then "iconst_1" else "iconst_0") ]
  | BOX_BOOL ->
      [ "invokestatic Method java/lang/Boolean valueOf (Z)Ljava/lang/Boolean;" ]
  | UNBOX_BOOL -> [ "invokevirtual Method java/lang/Boolean booleanValue ()Z" ]
  | PUSH_UNIT -> [ "getstatic Field Unit INSTANCE LUnit;" ]
  | BOP bop -> lower_bop ctrl_gen bop
  | STORE_REF r -> [ sprintf "astore %s" r ]
  | LOAD_REF r -> [ sprintf "aload %s" r ]
  | IFZERO l -> [ sprintf "ifeq %s" l ]
  | IFNONZERO l -> [ sprintf "ifne %s" l ]
  | GOTO l -> [ sprintf "goto %s" l ]
  | LABEL l -> [ sprintf "%s:" l ]
  | LOAD_FIELD (f, ty) ->
      [
        sprintf "getfield Field %s %s %s" clazz f (lower_type_as_descriptor ty);
      ]
  | STORE_FIELD (f, ty) ->
      [
        sprintf "putfield Field %s %s %s" clazz f (lower_type_as_descriptor ty);
      ]
  | ALLOC_OBJ name -> [ sprintf "new %s" name; "dup" ]
  | CONSTRUCT_OBJ (name, tys) ->
      [
        sprintf "invokespecial Method %s <init> (%s)V" name
          (lower_type_list tys);
      ]
  | ALLOC_ARRAY name -> [ sprintf "anewarray %s" name ]
  | STORE_ARRAY -> [ "aastore" ]
  | DUP -> [ "dup" ]
  | APPLY ty ->
      [
        "invokeinterface InterfaceMethod java/util/function/Function apply \
         (Ljava/lang/Object;)Ljava/lang/Object; 2";
        sprintf "checkcast %s" (lower_type ty);
      ]
  | LOAD_STATIC (clazz, f, ty) ->
      [ sprintf "getstatic Field %s %s L%s;" clazz f (lower_type ty) ]
  | STORE_STATIC (clazz, f, ty) ->
      [ sprintf "putstatic Field %s %s L%s;" clazz f (lower_type ty) ]

let should_indent = function LABEL _ -> false | _ -> true

let lower_body indent clazz b =
  let ctrl_gen = make_jvm_ctrl_gen () in
  List.map (lower_instruction ctrl_gen clazz) b
  |> List.flatten
  (* slightly dodgy bodge to only apply indent to labels *)
  |> List.map (fun i ->
         if String.starts_with ~prefix:"L" i then i else indent ^ i)
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
        sprintf "aload_%i" (i + 1);
        sprintf "putfield Field %s %s L%s;" name field (lower_type ty);
      ]
      |> List.map (fun i -> indent ^ i)
      |> String.concat "\n")
    constructor_args
  |> String.concat "\n"

(* todo - determine actual limits for stack and local variable sizes *)
let lower_closure (c : closure) =
  let indent = "    " in
  sprintf
    {|
.class public %s
.super java/lang/Object
.implements java/util/function/Function
%s

.method <init> : (%s)V
  .code stack 10 locals 10
    aload_0
    invokespecial Method java/lang/Object <init> ()V
%s
    return
  .end code
.end method

.method public apply : (Ljava/lang/Object;)Ljava/lang/Object;
  .code stack 10 locals 10
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
    (lower_constructor_body indent c.name c.constructor_args)
    (lower_type c.arg_type)
    (lower_body "    " c.name c.body)

let lower_type_interface (ti : type_interface) =
  sprintf
    {|
.version 62 0
.class public interface abstract %s
.super java/lang/Object
.permittedsubclasses %s
.end class
|}
    ti.name
    (String.concat " " ti.constructors)

let lower_value_constructor (vc : constructor) =
  sprintf
    {|
.version 62 0
.class public final super %s
.super java/lang/Object
.implements %s
%s

.method public <init> : (%s)V
    .code stack 2 locals 3
L0:     aload_0
L1:     invokespecial Method java/lang/Object <init> ()V
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
    (Option.map
       (fun arg ->
         [
           "aload_0";
           "aload_1";
           sprintf "putfield Field %s val L%s;" vc.name (lower_type arg);
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
           sprintf "getfield Field %s val L%s;" vc.name (lower_type arg);
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
L21:    getfield Field %s val L%s;
L24:    aload_1
L25:    checkcast %s
L28:    getfield Field %s val L%s;
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

let lower_field_defs p =
  List.filter_map
    (function STORE_STATIC (_, f, ty) -> Some (f, ty) | _ -> None)
    p
  |> List.map (fun (f, ty) ->
         sprintf ".field public static %s L%s;" f (lower_type ty))
  |> String.concat "\n"

let produce_instruction_bytecode p =
  sprintf
    {|
.class public Foo
.super java/lang/Object
%s
.method public static main : ([Ljava/lang/String;)V
    .code stack 100 locals 100
%s
        return
  .end code
.end method
.end class
|}
    (lower_field_defs p)
    (lower_body "        " "Foo" p)

let stdlib =
  {|
; my custom tuple class
.version 62 0
.class public super Tuple
.super java/lang/Object
.field public data [Ljava/lang/Object;

.method <init> : ([Ljava/lang/Object;)V
    .code stack 2 locals 2
L0:     aload_0
L1:     invokespecial Method java/lang/Object <init> ()V
L4:     aload_0
L5:     aload_1
L6:     putfield Field Tuple data [Ljava/lang/Object;
L9:     return

    .end code
.end method

.method public toString : ()Ljava/lang/String;
    .code stack 1 locals 1
L0:     aload_0
L1:     getfield Field Tuple data [Ljava/lang/Object;
L4:     invokestatic Method java/util/Arrays toString ([Ljava/lang/Object;)Ljava/lang/String;
L7:     areturn

    .end code
.end method

.method public equals : (Ljava/lang/Object;)Z
    .code stack 2 locals 2
L0:     aload_0
L1:     getfield Field Tuple data [Ljava/lang/Object;
L4:     aload_1
L5:     checkcast Tuple
L8:     getfield Field Tuple data [Ljava/lang/Object;
L11:    invokestatic Method java/util/Arrays equals ([Ljava/lang/Object;[Ljava/lang/Object;)Z
L14:    ireturn

    .end code
.end method

.method public hashCode : ()I
    .code stack 1 locals 1
L0:     aload_0
L1:     getfield Field Tuple data [Ljava/lang/Object;
L4:     invokestatic Method java/util/Arrays hashCode ([Ljava/lang/Object;)I
L7:     ireturn

    .end code
.end method
.sourcefile "Tuple.java"
.end class

.class public super Unit
.super java/lang/Object
.field public static INSTANCE LUnit;

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
L3:
        .linenumbertable
            L0 7
        .end linenumbertable
        .localvariabletable
            0 is this LUnit; from L0 to L3
        .end localvariabletable
    .end code
.end method

.method static <clinit> : ()V
    .code stack 2 locals 0
      new Unit
      dup
      invokespecial Method Unit <init> ()V
      putstatic Field Unit INSTANCE LUnit;
      return
    .end code
.end method
.end class

.version 62 0
.class public super Std
.super java/lang/Object
.field public static print Ljava/util/function/Function; .fieldattributes
    .signature Ljava/util/function/Function<Ljava/lang/Object;LUnit;>;
.end fieldattributes

.method public <init> : ()V
    .code stack 1 locals 1
L0:     aload_0
L1:     invokespecial Method java/lang/Object <init> ()V
L4:     return
L5:
        .linenumbertable
            L0 3
        .end linenumbertable
        .localvariabletable
            0 is this LStd; from L0 to L5
        .end localvariabletable
    .end code
.end method

.method private static synthetic lambda$static$0 : (Ljava/lang/Object;)LUnit;
    .code stack 2 locals 1
L0:     getstatic Field java/lang/System out Ljava/io/PrintStream;
L3:     aload_0
L4:     invokevirtual Method java/io/PrintStream println (Ljava/lang/Object;)V
L7:     getstatic Field Unit INSTANCE LUnit;
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

.method static <clinit> : ()V
    .code stack 1 locals 0
L0:     invokedynamic [_25]
L5:     putstatic Field Std print Ljava/util/function/Function;
L8:     return
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
.const [_25] = InvokeDynamic invokeStatic Method java/lang/invoke/LambdaMetafactory metafactory (Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite; MethodType (Ljava/lang/Object;)Ljava/lang/Object; [_59] MethodType (Ljava/lang/Object;)LUnit; : apply ()Ljava/util/function/Function;
.const [_59] = MethodHandle invokeStatic Method Std lambda$static$0 (Ljava/lang/Object;)LUnit;
.end class
|}

(*
--- MAIN METHOD FOR USING GENERATED CLOSURE ---

.method public static main : ([Ljava/lang/String;)V
    .code stack 3 locals 2
        new Lambda$1
        dup
        ldc 10
        invokestatic Method java/lang/Integer valueOf (I)Ljava/lang/Integer;
        invokespecial Method Lambda$1 <init> (Ljava/lang/Integer;)V
        ldc 3
        invokestatic Method java/lang/Integer valueOf (I)Ljava/lang/Integer;
        invokeinterface InterfaceMethod java/util/function/Function apply (Ljava/lang/Object;)Ljava/lang/Object; 2
        getstatic Field java/lang/System out Ljava/io/PrintStream;
        swap
        invokevirtual Method java/io/PrintStream println (Ljava/lang/Object;)V
        return
    .end code
.end method

*)
