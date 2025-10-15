(* codegen.ml - Python code generation *)

(* This module walks the AST and generates equivalent Python code.
   We use a simple recursive approach to convert each AST node to a string. *)

open Ast
open Encoding

(* Convert a binary operator to its Python equivalent *)
let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"  (* Note: in Python 3, this is float division *)

(* Convert an expression to Python code.
   Returns a string representing the Python expression. *)
let rec string_of_expr = function
  (* Ocaml ends whole floats in '.', not '.0' *)
  | Num n -> string_of_float n ^ "0"
  | Var x -> encode_prefix "BAST_"  x
  
  (* Binary operations: wrap in parentheses to preserve order of operations *)
  | BinOp (op, e1, e2) ->
      (* Recursively convert sub-expressions *)
      let left = string_of_expr e1 in
      let right = string_of_expr e2 in
      let op_str = string_of_binop op in
      (* Build the Python expression with explicit parentheses *)
      Printf.sprintf "(%s %s %s)" left op_str right

(* Convert a statement to Python code.
   Returns a string representing one or more Python statements. *)
let string_of_stmt = function
  (* Assignment: variable = expression *)
  | Assign (name, expr) ->
      Printf.sprintf "%s = %s" name (string_of_expr expr)
  
  (* Print: use Python's print() function *)
  | Print expr ->
      Printf.sprintf "print(%s)" (string_of_expr expr)

(* Convert an entire program to Python code.
   Takes a list of statements and joins them with newlines. *)
let string_of_program stmts =
  (* Add a Python shebang and encoding declaration for completeness *)
  let header = "#!/usr/bin/env python3\n# -*- coding: utf-8 -*-\n\n" in
  (* Convert each statement and join with newlines *)
  let body = String.concat "\n" (List.map string_of_stmt stmts) in
  header ^ body
