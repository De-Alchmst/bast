open Ast

let rec string_of_opmod = function
  | NoMod -> "NoMod"
  | UpTo   e -> Printf.sprintf "UpTo(%s)"   (string_of_expr e) 
  | DownTo e -> Printf.sprintf "DownTo(%s)" (string_of_expr e) 
  | ModTo  e -> Printf.sprintf "ModTo_to(%s)"   (string_of_expr e) 
  | InRange (e1, e2) -> Printf.sprintf "InRange(%s, %s)"
      (string_of_expr e1) (string_of_expr e2)
  | LoopInRange (e1, e2) -> Printf.sprintf "LoopInRange(%s, %s)"
      (string_of_expr e1) (string_of_expr e2)

and string_of_binop = function
  | Add m -> "op_val_add(" ^ (string_of_opmod m)
  | Sub m -> "op_val_sub(" ^ (string_of_opmod m)
  | Mul m -> "op_val_mul(" ^ (string_of_opmod m)
  | Div m -> "op_val_div(" ^ (string_of_opmod m)
  | Mod m -> "op_val_mod(" ^ (string_of_opmod m)
  | WholeDiv m -> "op_val_div_remles(" ^ (string_of_opmod m)

and string_of_unop = function
  | Plus  -> "val_plus"
  | Minus -> "val_minus"

and string_of_specvar = function
  | "nil" -> "Nil"
  | "f+"  -> "Fun(val_add, 2)"
  | "f-"  -> "Fun(val_sub, 2)"
  | "f*"  -> "Fun(val_mul, 2)"
  | "f//" -> "Fun(val_div_remles, 2)"
  | "f/"  -> "Fun(val_div, 2)"
  | "f%"  -> "Fun(val_mod, 2)"
  | _     -> "Nil"

and string_of_expr = function
  (* Ocaml ends whole floats in '.', not '.0' *)
  | Nil   -> "Nil"
  | Num n -> "Num(" ^ (string_of_float n) ^ "0)"
  | Var x -> (Encoding.encode_prefix x) ^ ".val"
  | SpecVar x -> string_of_specvar x

  | Assign (n, expr) ->
      let name = Encoding.encode_prefix n in
      Printf.sprintf "ass_var(%s,%s)"
        name (string_of_expr expr)

  | PostAssign (n, expr) ->
      let name = Encoding.encode_prefix n in
      Printf.sprintf "pos_ass_var(%s,%s)"
        name (string_of_expr expr)
  
  | BinOp (op, e1, e2) ->
      (* Recursively convert sub-expressions *)
      let op_str = string_of_binop op in
      let left   = string_of_expr  e1 in
      let right  = string_of_expr  e2 in
      Printf.sprintf "%s,[%s,%s])" op_str left right

  | UnOp (op, e) ->
      let op_str = string_of_unop op in
      let exp    = string_of_expr e  in
      Printf.sprintf "%s(%s)" op_str exp

  | Block (s, e) ->
      Printf.sprintf "{\n %s\n %s\n }"
        (String.concat "\n " (List.map string_of_stmt s))
        (string_of_expr e)

  | VarFunc (v, b) ->
      Printf.sprintf "call_var_func(%s,[%s])"
        (Encoding.encode_prefix v) (String.concat "," (List.map string_of_expr b))

  | ValFunc (e, b) ->
      Printf.sprintf "call_fun(%s,[%s])"
        (string_of_expr e) (String.concat "," (List.map string_of_expr b))

and string_of_stmt = function
  | Print expr ->
      Printf.sprintf "println(%s)" (string_of_expr expr)

  | Declare name ->
      Printf.sprintf "let %s=Var::{name:\"%s\",val:Nil}"
        (Encoding.encode_prefix name) name

  | ExprStmt expr ->
      Printf.sprintf "let _=%s" (string_of_expr expr)

  | Return expr ->
      Printf.sprintf "return %s" (string_of_expr expr)


and string_of_ast ast =
  " " ^ String.concat "\n " (List.map string_of_stmt ast)
