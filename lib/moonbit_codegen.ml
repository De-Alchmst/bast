open Ast
open Printf

let toplevel_declare = ref ""

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
  | Pow m      -> "op_val_pow(" ^ (string_of_opmod m)
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
  | Join       -> "val_join("

and string_of_args args =
  let rec aux acc n = function
    | [] -> acc
    | SimpleArg s :: rest ->
        let line = sprintf "let %s=Var::{name:\"%s\",val:argv[%d]}"
            (Encoding.encode_prefix s) s n in
        aux (acc ^ "\n " ^ line) (n + 1) rest
  in aux "" 0 args

and string_of_unop = function
  | Not    -> "val_not"
  | Negate -> "val_neg"

and value_of_type str = function
  | NoType -> str^";Nil"
  | I32 | I64 | U32 | U64 | F32 | F64 -> "Num("^str^".to_double())"

and pass_args_of_args args =
  let rec aux count = function
    | [] -> []
    | (_, ht) :: t -> (count, ht) :: (aux (count + 1) t)
  in
  String.concat ", " (List.map (function
    | (cnt, typ) ->
        sprintf "{match argv[%d] { Num(x) => x%s\n _ => panic()}}" cnt
        (match typ with
          | NoType -> "XXX"
          | I32    -> ".to_int()"
          | I64    -> ".to_int64()"
          | U32    -> ".to_uint()"
          | U64    -> ".to_uint64()"
          | F32    -> ".to_float()"
          | F64    -> ""))
  (aux 0 args))

and string_of_specvar = function
  | "nil"        -> "Nil"
  | "f+"         -> "Fun(val_add, 2)"
  | "f-"         -> "Fun(val_sub, 2)"
  | "f*"         -> "Fun(val_mul, 2)"
  | "f/"         -> "Fun(val_div, 2)"
  | "f%"         -> "Fun(val_mod, 2)"
  | "f^"         -> "Fun(val_pow, 2)"
  | "f//"        -> "Fun(val_div_remles, 2)"
  | "f&&"        -> "Fun(val_and, 2)"
  | "f||"        -> "Fun(val_or , 2)"
  | "f^^"        -> "Fun(val_xor, 2)"
  | "f!"         -> "Fun(val_a not, 1)"
  | "f<"         -> "Fun(val_lower, 2)"
  | "f<="        -> "Fun(val_lower_eq, 2)"
  | "f>"         -> "Fun(val_greater_eq, 2)"
  | "f>="        -> "Fun(val_greater_eq, 2)"
  | "f="         -> "Fun(val_equal, 2)"
  | "f!="        -> "Fun(val_not_equal, 2)"
  | "f~"         -> "Fun(val_join, 2)"
  | "println"    -> "Fun(val_println, 1)"
  | "cons"       -> "Fun(val_cons, 2)"
  | "car"        -> "Fun(val_car, 1)"
  | "cdr"        -> "Fun(val_cdr, 1)"
  | "true"       -> "Boo(true)"
  | "false"      -> "Boo(false)"
  | "nil?"       -> "Fun(val_nil_p, 1)"
  | "num?"       -> "Fun(val_num_p, 1)"
  | "atom?"      -> "Fun(val_atom_p, 1)"
  | "bool?"      -> "Fun(val_bool_p, 1)"
  | "func?"      -> "Fun(val_func_p, 1)"
  | "cons?"      -> "Fun(val_cons_p, 1)"
  | "list?"      -> "Fun(val_list_p, 1)"
  | "array?"     -> "Fun(val_array_p, 1)"
  | "string?"    -> "Fun(val_string_p, 1)"
  | "len"        -> "Fun(val_len, 1)"
  | "to-string"  -> "Fun(val_to_string, 1)"
  | "to-debug"   -> "Fun(val_to_debug, 1)"
  | "panic"      -> "Fun(val_panic, 1)"
  | "array-make" -> "Fun(val_array_make, 2)"
  | "push"       -> "Fun(val_push, 2)"
  | "push!"      -> "Fun(val_push_dest, 2)"
  | "pop"        -> "Fun(val_pop, 1)"
  | "pop!"       -> "Fun(val_pop_dest, 1)"
  | "insert"     -> "Fun(val_insert, 3)"
  | "insert!"    -> "Fun(val_insert_dest, 3)"
  | "remove"     -> "Fun(val_remove, 2)"
  | "remove!"    -> "Fun(val_remove_dest, 2)"
  | "split"      -> "Fun(val_split, 2)"
  | "chars"      -> "Fun(val_chars, 1)"
  | "arity"      -> "Fun(val_arity, 1)"
  | _            -> "Nil"

and string_of_expr = function
  (* Ocaml ends whole floats in '.', not '.0' *)
  | Nil   -> "Nil"
  | Num n -> "Num(" ^ (string_of_float n) ^ "0)"
  | Var x -> (Encoding.encode_prefix x) ^ ".val"
  | SpecVar x -> string_of_specvar x
  | Str v -> "Str(" ^v^ ")"

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

  | Cxr s ->
      sprintf "Fun(cxr_to_func(\"%s\"), 1)" (Encoding.string_rev s) 

  | If (cond, t, f) ->
      sprintf "if val_to_bool(%s) %s else %s"
        (string_of_expr cond) (string_of_expr t) (string_of_expr f)

  | Cond lst ->
      (lst |> List.map(function | (cond, bod) ->
        sprintf "if val_to_bool(%s)%s\n else "
          (string_of_expr cond) (string_of_expr bod))
      |> String.concat "") ^ "{Nil}"

  | While (cond, dec, body) ->
      sprintf "{let mut _rval=Nil;while val_to_bool(%s){_rval={\n %s\n %s\n }};_rval}"
        (string_of_expr cond) (string_of_stmt dec) (string_of_expr body)

  | DoWhile (cond, dec, body) ->
      sprintf "{let mut _rval=Nil;while true{_rval={\n %s\n %s\n };if !val_to_bool(%s){break}};_rval}"
        (string_of_stmt dec) (string_of_expr body) (string_of_expr cond)

  | For (ftype, ind, from, upto, dec, body) ->
      let pref_ind = Encoding.encode_prefix ind in
      sprintf "{let mut _rval=Nil;let %s:Var={name:\"%s\",val:%s};while val_to_bool(%s([%s.val,%s])){_rval={\n %s\n %s\n };%s.val=%s([%s.val, Num(1.0)])};_rval}"
        pref_ind ind (string_of_expr from)
        (match ftype with | Ascending -> "val_lower_eq" | Descending -> "val_greater_eq")
        pref_ind (string_of_expr upto)
        (string_of_stmt dec) (string_of_expr body)
        pref_ind
        (match ftype with | Ascending -> "val_add" | Descending -> "val_sub")
        pref_ind

  | Lambda (args, dec, body) ->
      sprintf "Fun(fn (argv: Array[Value]) -> Value {%s\n %s\n %s}, %d)"
        (string_of_args args) 
        (string_of_stmt dec)
        (string_of_expr body)
        (List.length args)

  | Array e ->
      sprintf "Arr([%s])"
        (String.concat "," (List.map string_of_expr e))

  | Read (src, ind) ->
      sprintf "val_read(%s, [%s])"
        (string_of_expr src)
        (String.concat "," (List.map string_of_expr ind)) 

  | Write (src, ind, n) ->
      sprintf "val_write(%s, [%s], %s)"
        (string_of_expr src)
        (String.concat "," (List.map string_of_expr ind)) 
        (string_of_expr n)

and string_of_stmt = function
  | Declare (name, e) ->
      let pref_name = Encoding.encode_prefix name in
      if e = Nil then
        sprintf "let %s:Var={name:\"%s\",val:Nil}" pref_name name
      else (* lambdas cannot refer to the variable if declared within *)
        sprintf "let %s:Var={name:\"%s\",val:Nil}; %s.val=%s"
        pref_name name pref_name (string_of_expr e)

  | ToplevelDeclare (name, e) ->
      let pref_name = Encoding.encode_prefix name in
      toplevel_declare :=
        !toplevel_declare ^ sprintf "let %s:Var={name:\"%s\", val:Nil}\n"
          pref_name name;

      sprintf "%s.val = %s" pref_name (string_of_expr e)

  | StmtList stmts ->
      String.concat "\n " (List.map string_of_stmt stmts)

  | ExprStmt expr ->
      sprintf "let _=%s" (string_of_expr expr)

  | Return expr ->
      sprintf "return %s" (string_of_expr expr)

  | ExportDeclareFunc (name, body) ->
      let pref_name = Encoding.encode_prefix name in
      let exp_name  = Encoding.encode_export_prefix name in
      toplevel_declare :=
        !toplevel_declare ^ sprintf "let %s:Var={name:\"%s\", val:Nil}\n"
          pref_name name;

      Moonbit_conf.export_functions :=
        (exp_name, pref_name) :: !Moonbit_conf.export_functions;

      sprintf "%s.val = %s" pref_name (string_of_expr body)

  | ExternalFuncDeclare (name, args, ret, ext) ->
      let pref_name = Encoding.encode_prefix name in
      let ext_name = Encoding.encode_external_prefix name in
      toplevel_declare :=
        !toplevel_declare ^ sprintf "let %s:Var={name:\"%s\", val:Nil}\n"
          pref_name name;

      Moonbit_conf.extern_functions :=
        (ext_name, args, ret, ext) :: !Moonbit_conf.extern_functions;

      sprintf "%s.val = Fun(fn (argv: Array[Value]) -> Value {%s}, %d)"
      pref_name
      (value_of_type
        (sprintf "%s(%s)"
          ext_name
          (pass_args_of_args args))
        ret)
      (List.length args)


and string_of_ast ast =
  toplevel_declare := "";
  let body = List.map string_of_stmt ast in
  "" ^ !toplevel_declare ^ "fn init {" ^ String.concat "\n " body ^ "\n} "
