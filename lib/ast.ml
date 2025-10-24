(* ast.ml - Abstract Syntax Tree definition *)

(* This file defines the data structures that represent our language.
   After parsing, the source code is converted into these structures. *)

(* Binary operators for arithmetic *)
type opmod =
  | NoMod
  | UpTo of expr
  | DownTo of expr
  | ModTo of expr
  | InRange of expr * expr
  | LoopInRange of expr * expr

and binop =
  | Add of opmod
  | Sub of opmod
  | Mul of opmod
  | Div of opmod
  | Mod of opmod
  | WholeDiv of opmod
  | Equals
  | NotEquals
  | Greater
  | Lesser
  | GreaterEq
  | LesserEq
  | And
  | Or
  | Xor

and unop =
  | Plus (* nop *)
  | Minus
  | Not

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
  | PostAssign of string * expr (* assign, but return the old value *)
  | If of expr * expr * expr (* cont true false *)
  | While of expr * stmt * expr (* condition, declaration, body *)

(* Statements - things that do actions *)
and stmt = (* 'and' for mutually recursive types *)
  | Print of expr
  | Declare of string * expr 
  | StmtList of stmt list
  | ExprStmt of expr
  | Return of expr


(* A program is a list of statements *)
type program = stmt list
