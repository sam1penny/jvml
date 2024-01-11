open Linear.Instruction
open Printf
open Common

let lower_type =
  let open Typing.Typed_ast in
  function
  | TyInt -> "java/lang/Integer"
  | TyBool -> "java/lang/Boolean"
  | TyFun _ -> "java/util/function/Function"
  | TyVar _ -> "java/lang/Object"
  | TyUnit -> "Unit"
  | _ -> raise @@ Failure "attempted to lower unsupported type"

let lower_type_list tys =
  List.map (fun t -> "L" ^ lower_type t ^ ";") tys |> String.concat ""

let lower_bop = function
  | ADD -> "iadd"
  | SUB -> "isub"
  | MUL -> "imul"
  | DIV -> "idiv"
  | AND -> "iand"
  | OR -> "ior"
  | _ -> "attempted to lower unsupported binary operator"

let lower_instruction clazz = function
  | PUSH_INT i -> [ sprintf "ldc %s" (string_of_int i) ]
  | BOX_INT ->
      [ "invokestatic Method java/lang/Integer valueOf (I)Ljava/lang/Integer;" ]
  | UNBOX_INT -> [ "invokevirtual Method java/lang/Integer intValue ()I" ]
  | PUSH_UNIT -> [ "getstatic Field Unit INSTANCE LUnit;" ]
  | BOP bop -> [ lower_bop bop ]
  | STORE_REF r -> [ sprintf "astore %s" r ]
  | LOAD_REF r -> [ sprintf "aload %s" r ]
  | IFZERO l -> [ sprintf "ifeq %s" l ]
  | GOTO l -> [ sprintf "goto %s" l ]
  | LABEL l -> [ sprintf "%s:" l ]
  | LOAD_FIELD (f, ty) ->
      [ sprintf "getfield Field %s %s L%s;" clazz f (lower_type ty) ]
  | STORE_FIELD (f, ty) ->
      [ sprintf "putfield Field %s %s L%s;" clazz f (lower_type ty) ]
  | ALLOC_CLOSURE name -> [ sprintf "new %s" name; "dup" ]
  (* todo - link to actual closure arguments *)
  | CONSTRUCT_CLOSURE (name, tys) ->
      [
        sprintf "invokespecial Method %s <init> (%s)V" name
          (lower_type_list tys);
      ]
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

let lower_body indent clazz b =
  List.map (lower_instruction clazz) b
  |> List.flatten
  |> List.map (fun x -> indent ^ x)
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
let lower_closure = function
  | CLOSURE (name, constructor_args, arg, _, body) ->
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
.end class
    |}
        name
        (lower_constructor_args constructor_args)
        (List.map (fun (_, ty) -> ty) constructor_args |> lower_type_list)
        (lower_constructor_body indent name constructor_args)
        (lower_type arg)
        (lower_body "    " name body)

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
    .code stack 13 locals 13
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
