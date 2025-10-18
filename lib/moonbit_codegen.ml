open Ast

let string_of_binop = function
  | Add -> "val_add"
  | Sub -> "val_sub"
  | Mul -> "val_mul"
  | Div -> "val_div"
let rec string_of_expr = function
  (* Ocaml ends whole floats in '.', not '.0' *)
  | Num n -> "Num(" ^ (string_of_float n) ^ "0)"
  | Var x -> (Encoding.encode_prefix x) ^ ".val"
  
  | BinOp (op, e1, e2) ->
      (* Recursively convert sub-expressions *)
      let left = string_of_expr e1 in
      let right = string_of_expr e2 in
      let op_str = string_of_binop op in
      Printf.sprintf "%s([%s,%s])" op_str left right

let string_of_stmt = function
  | Assign (name, expr) ->
      Printf.sprintf "%s.val=%s" (Encoding.encode_prefix name) (string_of_expr expr)

  | Print expr ->
      Printf.sprintf "println(%s)" (string_of_expr expr)

  | Declare name ->
      Printf.sprintf "let %s=Var::{name:\"%s\",val:Nil}" (Encoding.encode_prefix name) name

  | ExprStmt expr ->
      Printf.sprintf "let _=%s" (string_of_expr expr)


let string_of_ast ast =
  " " ^ String.concat "\n " (List.map string_of_stmt ast)
