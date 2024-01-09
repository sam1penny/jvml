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
  | _ -> raise @@ Failure "attempted to lower unsupported type"

let lower_bop = function
  | ADD -> "iadd"
  | SUB -> "isub"
  | MUL -> "imul"
  | DIV -> "idiv"
  | AND -> "iand"
  | OR -> "ior"
  | _ -> "attempted to lower unsupported binary operator"

let lower_instruction = function
  | PUSH_INT i -> [ sprintf "ldc %s" (string_of_int i) ]
  | BOX_INT ->
      [ "invokestatic Method java/lang/Integer valueOf (I)Ljava/lang/Integer;" ]
  | UNBOX_INT -> [ "invokevirtual Method java/lang/Integer intValue ()I" ]
  | BOP bop -> [ lower_bop bop ]
  | STORE_REF r -> [ sprintf "astore %s" r ]
  | LOAD_REF r -> [ sprintf "aload %s" r ]
  | IFZERO l -> [ sprintf "ifeq %s" l ]
  | GOTO l -> [ sprintf "goto %s" l ]
  | LABEL l -> [ sprintf "%s:" l ]
  (* todo - encode closure class name and field types *)
  | LOAD_FIELD f ->
      [ sprintf "getfield Field Lambda$1 %s Ljava/lang/Integer;" f ]
  | ALLOC_CLOSURE name -> [ sprintf "new %s" name; "dup" ]
  (* todo - link to actual closure arguments *)
  | CONSTRUCT_CLOSURE (name, _) ->
      [ sprintf "invokespecial Method %s <init> (Ljava/lang/Integer;)V" name ]
  (* todo - cast result? *)
  | APPLY ->
      [
        "invokeinterface InterfaceMethod java/util/function/Function apply \
         (Ljava/lang/Object;)Ljava/lang/Object; 2";
      ]
  (* todo - add support for remaining instructions EQ, STORE_FIELD *)
  | _ -> raise @@ Invalid_argument "Unsupported instruction"

let lower_body indent b =
  List.map lower_instruction b
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
        (List.map (fun (_, ty) -> ty) constructor_args
        |> List.map (fun t -> "L" ^ lower_type t ^ ";")
        |> String.concat "")
        (lower_constructor_body indent name constructor_args)
        (lower_type arg) (lower_body "    " body)

let produce_instruction_bytecode p =
  sprintf
    {|
.class public Foo
.super java/lang/Object
.method public static main : ([Ljava/lang/String;)V
    .code stack 13 locals 13
        getstatic Field java/lang/System out Ljava/io/PrintStream;
%s
        aload 2
        invokevirtual Method java/io/PrintStream println (Ljava/lang/Object;)V
        return
  .end code
.end method
.end class
|}
    (lower_body "        " p)

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
