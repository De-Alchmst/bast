(* parser.mly - Grammar specification for Menhir *)

(* This header section contains OCaml code included in the generated parser *)
%{
  open Ast  (* We need the AST types we defined *)
%}

(* Token declarations - these define all possible tokens the lexer can produce.
   Tokens with <type> carry additional data. *)
%token <int> INT        (* Integer literals carry their value *)
%token <string> IDENT   (* Identifiers carry their name *)
%token PLUS MINUS TIMES DIVIDE  (* Arithmetic operators *)
%token EQUALS           (* Assignment operator *)
%token LPAREN RPAREN    (* Parentheses *)
%token PRINT            (* Print keyword *)
%token EOF              (* End of file *)

(* Operator precedence and associativity - lower declarations = lower precedence.
   This tells Menhir how to resolve ambiguities like "1 + 2 * 3" *)
%left PLUS MINUS        (* + and - are left-associative and lowest precedence *)
%left TIMES DIVIDE      (* * and / are left-associative and higher precedence *)

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

(* A statement is either an assignment or a print statement *)
stmt:
  (* Assignment: identifier = expression *)
  | name = IDENT; EQUALS; e = expr
      { Assign (name, e) }
  
  (* Print statement: print expression *)
  | PRINT; e = expr
      { Print e }

(* Expression grammar - builds up expression AST nodes *)
expr:
  (* Integer literal *)
  | n = INT
      { Num n }
  
  (* Variable reference *)
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