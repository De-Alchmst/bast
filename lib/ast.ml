(* ast.ml - Abstract Syntax Tree definition *)

(* This file defines the data structures that represent our language.
   After parsing, the source code is converted into these structures. *)

(* Binary operators for arithmetic *)
type opmod =
  | NoMod
  | OpNum of expr

and binop =
  | Add of opmod
  | Sub of opmod
  | Mul of opmod
  | Div of opmod
  | Mod of opmod
  | WholeDiv of opmod

and unop =
  | Plus (* nop *)
  | Minus

(* Expressions - things that evaluate to values *)
and expr =
  | Nil
  | Num of float
  | Var of string
  | SpecVar of string
  | BinOp of binop * expr * expr
  | UnOp of unop * expr
  | Block of stmt list * expr (* stetements + return *)
  | VarFunc of string * expr list (* contains var name data for errors *)
  | ValFunc of expr * expr list
  | Assign of string * expr
  (* | VarOp of string * binop * expr *)

(* Statements - things that do actions *)
and stmt = (* 'and' for mutually recursive types *)
  | Print of expr
  | Declare of string
  | ExprStmt of expr
  | Return of expr

(* A program is a list of statements *)
type program = stmt list
