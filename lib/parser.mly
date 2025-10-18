(* parser.mly - Grammar specification for Menhir *)

(* This header section contains OCaml code included in the generated parser *)
%{
  open Ast  (* We need the AST types we defined *)
%}

%token <float> NUM
%token <string> IDENT
%token BIND
%token PLUS MINUS TIMES DIVIDE WHOLE_DIVIDE MODULO
%token CONS
%token EQUALS VAR
%token LPAREN RPAREN LSQUARE RSQUARE LCURLY RCURLY
%token EOF

%token PRINT

(* lower declarations = lower precedence. *)
   
%right CONS
%left PLUS MINUS
%left TIMES DIVIDE WHOLE_DIVIDE MODULO

(* The start symbol - what the parser tries to parse.
   <Ast.program> is the type that this rule returns. *)
%start <Ast.program> prog

%%

(* Grammar rules follow. Format is:
   rule_name:
     | pattern1 { action1 }
     | pattern2 { action2 }
   
   Actions are OCaml code that builds AST nodes. *)

(* A program is a list of statements followed by EOF *)
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

  | e = expr
      { ExprStmt e }

(* Expression grammar - builds up expression AST nodes *)
expr:
  | n = NUM
      { Num n }
  
  | x = IDENT
      { Var x }
  
  (* Parenthesized expression - just returns the inner expression *)
  | LPAREN; e = expr; RPAREN
      { e }
  
  (* Binary operations - precedence is handled by the %left declarations *)
  | e1 = expr; PLUS; e2 = expr
      { BinOp (Add, e1, e2) }
  
  | e1 = expr; MINUS; e2 = expr
      { BinOp (Sub, e1, e2) }
  
  | e1 = expr; TIMES; e2 = expr
      { BinOp (Mul, e1, e2) }
  
  | e1 = expr; DIVIDE; e2 = expr
      { BinOp (Div, e1, e2) }
