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
  | Num of float
  | Var of string
  | BinOp of binop * expr * expr

(* Statements - things that do actions *)
type stmt =
  | Assign of string * expr  (* Variable assignment: x = 10 *)
  | Print of expr            (* Print statement: print x + 5 *)

(* A program is a list of statements *)
type program = stmt list
