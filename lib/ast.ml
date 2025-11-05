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
  | Negate
  | Not

and arg =
  | SimpleArg of string

and for_type =
  | Ascending
  | Descending

and func_type =
  | NoType | I32 | I64 | U32 | U64 | F32 | F64

(* Expressions - things that evaluate to values *)
and expr =
  | Nil
  | Num of float
  | Str of string
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
  | Cond of (expr * expr) list (* condition, body *)
  | While of expr * stmt * expr (* condition, declaration, body *)
  | DoWhile of expr * stmt * expr (* condition, declaration, body *)
  | Cons of expr * expr (* head, tail *)
  | Cxr of string
  | Lambda of arg list * stmt * expr (* args, declaration, body *)
  | For of for_type * string * expr * expr * stmt * expr  (* var, start, end, declaration, body *)
  | Array of expr list
  | Read of expr * expr list (* source, index *)
  | Write of expr * expr list * expr (* source, index, new value *)

(* Statements - things that do actions *)
and stmt = (* 'and' for mutually recursive types *)
  | Declare of string * expr 
  | StmtList of stmt list
  | ExprStmt of expr
  | Return of expr
  | ToplevelDeclare of string * expr
  | ExternalFuncDeclare of string * (string * func_type) list * func_type * string list
  | ExportDeclareFunc of string * expr


(* A program is a list of statements *)
type program = stmt list
