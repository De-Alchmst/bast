open Ast
open Printf

let rec string_of_opmod = function
  | NoMod    -> "NoMod,"
  | UpTo   e -> sprintf "UpTo(%s),"   (string_of_expr e) 
  | DownTo e -> sprintf "DownTo(%s)," (string_of_expr e) 
  | ModTo  e -> sprintf "ModTo(%s),"  (string_of_expr e) 
  | InRange (e1, e2) -> sprintf "InRange(%s, %s),"
      (string_of_expr e1) (string_of_expr e2)
  | LoopInRange (e1, e2) -> sprintf "LoopInRange(%s, %s),"
      (string_of_expr e1) (string_of_expr e2)

and string_of_binop = function
  | Add m      -> "op_val_add(" ^ (string_of_opmod m)
  | Sub m      -> "op_val_sub(" ^ (string_of_opmod m)
  | Mul m      -> "op_val_mul(" ^ (string_of_opmod m)
  | Div m      -> "op_val_div(" ^ (string_of_opmod m)
  | Mod m      -> "op_val_mod(" ^ (string_of_opmod m)
  | WholeDiv m -> "val_div_remles(" ^ (string_of_opmod m)
  | Equals     -> "val_equal("
  | NotEquals  -> "val_not_equal("
  | Greater    -> "val_greater("
  | Lesser     -> "val_lower("
  | GreaterEq  -> "val_greater_eq("
  | LesserEq   -> "val_lower_eq("
  | And        -> "val_and("
  | Or         -> "val_or("
  | Xor        -> "val_xor("


and string_of_unop = function
  | Plus  -> "val_plus"
  | Minus -> "val_minus"
  | Not   -> "val_not"

and string_of_specvar = function
  | "nil"     -> "Nil"
  | "f+"      -> "Fun(val_add, 2)"
  | "f-"      -> "Fun(val_sub, 2)"
  | "f*"      -> "Fun(val_mul, 2)"
  | "f//"     -> "Fun(val_div_remles, 2)"
  | "f/"      -> "Fun(val_div, 2)"
  | "f%"      -> "Fun(val_mod, 2)"
  | "f&&"     -> "Fun(val_and, 2)"
  | "f||"     -> "Fun(val_or , 2)"
  | "f^^"     -> "Fun(val_xor, 2)"
  | "f!"      -> "Fun(val_a not, 1)"
  | "f<"      -> "Fun(val_lower, 2)"
  | "f<="     -> "Fun(val_lower_eq, 2)"
  | "f>"      -> "Fun(val_greater_eq, 2)"
  | "f>="     -> "Fun(val_greater_eq, 2)"
  | "f="      -> "Fun(val_equal, 2)"
  | "f!="     -> "Fun(val_not_equal, 2)"
  | "cons"    -> "Fun(val_cons, 2)"
  | "car"     -> "Fun(val_car, 1)"
  | "cdr"     -> "Fun(val_cdr, 1)"
  | "true"    -> "Boo(true)"
  | "false"   -> "Boo(false)"
  | "nil?"    -> "Fun(val_nil_p, 1)"
  | "num?"    -> "Fun(val_num_p, 1)"
  | "atom?"   -> "Fun(val_atom_p, 1)"
  | "bool?"   -> "Fun(val_bool_p, 1)"
  | "func?"   -> "Fun(val_func_p, 1)"
  | "cons?"   -> "Fun(val_cons_p, 1)"
  | "list?"   -> "Fun(val_list_p, 1)"
  | "Array?"  -> "Fun(val_array_p, 1)"
  | "string?" -> "Fun(val_string_p, 1)"
  | _         -> "Nil"

and string_of_expr = function
  (* Ocaml ends whole floats in '.', not '.0' *)
  | Nil   -> "Nil"
  | Num n -> "Num(" ^ (string_of_float n) ^ "0)"
  | Var x -> (Encoding.encode_prefix x) ^ ".val"
  | SpecVar x -> string_of_specvar x

  | Assign (n, expr) ->
      let name = Encoding.encode_prefix n in
      sprintf "ass_var(%s,%s)"
        name (string_of_expr expr)

  | PostAssign (n, expr) ->
      let name = Encoding.encode_prefix n in
      sprintf "pos_ass_var(%s,%s)"
        name (string_of_expr expr)
  
  | BinOp (op, e1, e2) ->
      (* Recursively convert sub-expressions *)
      let op_str = string_of_binop op in
      let left   = string_of_expr  e1 in
      let right  = string_of_expr  e2 in
      sprintf "%s[%s,%s])" op_str left right

  | UnOp (op, e) ->
      let op_str = string_of_unop op in
      let exp    = string_of_expr e  in
      sprintf "%s(%s)" op_str exp

  | Block (s, e) ->
      sprintf "{\n %s\n %s\n }"
        (String.concat "\n " (List.map string_of_stmt s)) (string_of_expr e)

  | VarFunc (v, b) ->
      sprintf "call_var_func(%s,[%s])"
        (Encoding.encode_prefix v) (String.concat "," (List.map string_of_expr b))

  | ValFunc (e, b) ->
      sprintf "call_fun(%s,[%s])"
        (string_of_expr e) (String.concat "," (List.map string_of_expr b))

  | Cons (h, t) ->
      sprintf "Cons(%s,%s)" (string_of_expr h) (string_of_expr t)

  | Cxr _ ->
      sprintf "Fun(var_car, 1)"

  | If (cond, t, f) ->
      sprintf "if val_to_bool(%s) %s else %s"
        (string_of_expr cond) (string_of_expr t) (string_of_expr f)

  | While (cond, dec, body) ->
      sprintf "{let mut _rval=Nil;while val_to_bool(%s){_rval={\n %s\n %s\n }};_rval}"
        (string_of_expr cond) (string_of_stmt dec) (string_of_expr body)

and string_of_stmt = function
  | Print expr ->
      sprintf "println(%s)" (string_of_expr expr)

  | Declare (name, e) ->
      sprintf "let %s=Var::{name:\"%s\",val:%s}"
        (Encoding.encode_prefix name) name (string_of_expr e)

  | StmtList stmts ->
      String.concat "\n " (List.map string_of_stmt stmts)

  | ExprStmt expr ->
      sprintf "let _=%s" (string_of_expr expr)

  | Return expr ->
      sprintf "return %s" (string_of_expr expr)


and string_of_ast ast =
  " " ^ String.concat "\n " (List.map string_of_stmt ast)
