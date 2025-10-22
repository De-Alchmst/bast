(* parser.mly - Grammar specification for Menhir *)

(* This header section contains OCaml code included in the generated parser *)
%{
  open Ast  (* We need the AST types we defined *)
%}

%token <float> NUM
%token <string> IDENT
%token <string> SPECIAL_IDENT
%token INCREMENT DECREMENT
%token BIND PIPE
%token PLUS MINUS TIMES DIVIDE WHOLE_DIVIDE MODULO
%token LESSER GREATER
%token CONS
%token EQUALS VAR
%token LPAREN RPAREN LSQUARE RSQUARE LCURLY RCURLY
%token EOF

%token DO

%token PRINT
%token RETURN

(* lower declarations = lower precedence. *)
   
%right CONS
%left LESSER GREATER 
%left PLUS MINUS
%left TIMES DIVIDE WHOLE_DIVIDE MODULO
%nonassoc unary_minus unary_plus

(* The start symbol - what the parser tries to parse.
   <Ast.program> is the type that this rule returns. *)
%start <Ast.program> prog

%%

prog:
  | stmts = list(stmt); EOF
      { stmts }  (* Just return the list of statements *)


stmt:
  | PRINT; e = expr
      { Print e }

  | VAR; name = IDENT
      { Declare name }

  | RETURN; e = expr
      { Return e }

  | e = expr
      { ExprStmt e }

(* Expression grammar - builds up expression AST nodes *)
expr:
  | name = IDENT; BIND; EQUALS; e = expr
      { Assign (name, e) }

  (* assign binds *)
  | name = IDENT; BIND; op = bin_op; e = expr
      { Assign (name, BinOp (op, Var (name), e)) }

  | name = IDENT; BIND; INCREMENT; m = bin_op_mod
      { Assign (name, BinOp (Add (m), Var (name), Num 1.)) }
  | name = IDENT; BIND; DECREMENT; m = bin_op_mod
      { Assign (name, BinOp (Sub (m), Var (name), Num 1.)) }

  (* post assign binds *)
  | EQUALS; e = expr; BIND; name = IDENT
      { PostAssign (name, e) }

  | op = bin_op; e = expr; BIND; name = IDENT
      { PostAssign (name, BinOp (op, Var (name), e)) }

  | INCREMENT; m = bin_op_mod; BIND; name = IDENT
      { PostAssign (name, BinOp (Add (m), Var (name), Num 1.)) }
  | DECREMENT; m = bin_op_mod; BIND; name = IDENT
      { PostAssign (name, BinOp (Sub (m), Var (name), Num 1.)) }


  | n = NUM
      { Num n }
  
  | x = IDENT
      { Var x }

  | x = SPECIAL_IDENT
      { SpecVar x }
  
  (* Parenthesized expression - just returns the inner expression *)
  | LPAREN; e = expr; RPAREN
      { e }

  (* `s = list(stmt); e = expr` failed me, so we're doing this then I guess *)
  | DO; LSQUARE; s = list(stmt); RSQUARE
      { match (List.rev s) with
          | ExprStmt(e) :: t -> Block (List.rev t, e)
          | x -> Block (List.rev x, SpecVar "nil")}

  | LSQUARE; e = list(expr); RSQUARE
      { match e with
        | []         -> Block ([], SpecVar "nil")(* how to raise error again? *)
        | Var h :: t -> VarFunc (h, t)
        | h :: t     -> ValFunc (h, t)}
  
  (* cannot be factored, because percedence *)
  | e1 = expr; PLUS; m=bin_op_mod ; e2 = expr
    { BinOp ((Add m), e1, e2) }

  | e1 = expr; MINUS; m=bin_op_mod ; e2 = expr
    { BinOp ((Sub m), e1, e2) }

  | e1 = expr; TIMES; m=bin_op_mod ; e2 = expr
    { BinOp ((Mul m), e1, e2) }

  | e1 = expr; DIVIDE; m=bin_op_mod ; e2 = expr
    { BinOp ((Div m), e1, e2) }

  | e1 = expr; MODULO; m=bin_op_mod ; e2 = expr
    { BinOp ((Mod m), e1, e2) }

  | e1 = expr; WHOLE_DIVIDE; m=bin_op_mod ; e2 = expr
    { BinOp ((WholeDiv m), e1, e2) }


  (* Unary operations *)
  | MINUS; e = expr %prec unary_minus
      { UnOp (Minus, e)  }
    
  (* to allow -++--foo, since it's fun*)
  | PLUS     ; e = expr %prec unary_plus
  | INCREMENT; e = expr %prec unary_plus
  | DECREMENT; e = expr %prec unary_plus
      { UnOp (Plus, e) }


bin_op:
  | PLUS;         m = bin_op_mod { Add      m }
  | MINUS;        m = bin_op_mod { Sub      m }
  | TIMES;        m = bin_op_mod { Mul      m }
  | DIVIDE;       m = bin_op_mod { Div      m }
  | MODULO;       m = bin_op_mod { Mod      m }
  | WHOLE_DIVIDE; m = bin_op_mod { WholeDiv m }


bin_op_mod:
  | BIND; GREATER; BIND; e = expr
      { UpTo e }

  | BIND; LESSER; BIND; e = expr
      { DownTo e }

  | BIND; MODULO; BIND; e = expr
      { ModTo e }

  | BIND; n = NUM
      { UpTo (Num n) }

  | { NoMod }
