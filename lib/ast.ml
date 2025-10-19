(* ast.ml - Abstract Syntax Tree definition *)

(* This file defines the data structures that represent our language.
   After parsing, the source code is converted into these structures. *)

(* Binary operators for arithmetic *)
type binop =
  | Add
  | Sub
  | Mul
  | Div

(* Expressions - things that evaluate to values *)
type expr =
  | Nil
  | Num of float
  | Var of string
  | SpecVar of string
  | BinOp of binop * expr * expr
  | Block of stmt list * expr (* stetements + return *)

(* Statements - things that do actions *)
and stmt = (* 'and' for mutually recursive types *)
  | Assign of string * expr
  | Print of expr
  | Declare of string
  | ExprStmt of expr
  | Return of expr

(* A program is a list of statements *)
type program = stmt list
