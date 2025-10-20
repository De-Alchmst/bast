(* parser.mly - Grammar specification for Menhir *)

(* This header section contains OCaml code included in the generated parser *)
%{
  open Ast  (* We need the AST types we defined *)
%}

%token <float> NUM
%token <string> IDENT
%token <string> SPECIAL_IDENT
%token BIND
%token PLUS MINUS TIMES DIVIDE WHOLE_DIVIDE MODULO
%token CONS
%token EQUALS VAR
%token LPAREN RPAREN LSQUARE RSQUARE LCURLY RCURLY
%token EOF

%token DO

%token PRINT
%token RETURN

(* lower declarations = lower precedence. *)
   
%right CONS
%left PLUS MINUS
%left TIMES DIVIDE WHOLE_DIVIDE MODULO

(* The start symbol - what the parser tries to parse.
   <Ast.program> is the type that this rule returns. *)
%start <Ast.program> prog

%%

prog:
  | stmts = list(stmt); EOF
      { stmts }  (* Just return the list of statements *)

stmt:
  | name = IDENT; EQUALS; e = expr
      { Assign (name, e) }
  
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
  
  (* Binary operations - precedence is handled by the %left declarations *)
  | e1 = expr; op = bin_op ; e2 = expr
      { BinOp (op, e1, e2) }

  (* Unary operations *)
  | MINUS; e = expr
      { UnOp (Minus, e)  }
    
  | PLUS; e = expr
      { UnOp (Plus, e) }

bin_op:
  | PLUS           { Add NoMod }
  | MINUS          { Sub NoMod }
  | TIMES          { Mul NoMod }
  | DIVIDE         { Div NoMod }
  | MODULO         { Mod NoMod }
  | WHOLE_DIVIDE   { WholeDiv NoMod }
