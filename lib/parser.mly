(* parser.mly - Grammar specification for Menhir *)

(* This header section contains OCaml code included in the generated parser *)
%{
  open Ast  (* We need the AST types we defined *)
%}

%token <float> NUM
%token <string> IDENT
%token <string> SPECIAL_IDENT
%token <string> CXR
%token INCREMENT DECREMENT
%token BIND PIPE
%token PLUS MINUS TIMES DIVIDE WHOLE_DIVIDE MODULO
%token EQUALS NOT_EQUALS LESSER LESSER_OR_EQUAL GREATER GREATER_OR_EQUAL
%token NOT OR AND XOR
%token CONS
%token FUNC LAMBDA
%token VAR PRINT RETURN
%token DO IF UNLESS WHILE UNTIL
%token LPAREN RPAREN LSQUARE RSQUARE LCURLY RCURLY
%token EOF
   
%right CONS

%nonassoc unary_minus unary_plus
%left AND OR XOR
%left EQUALS NOT_EQUALS
%left LESSER GREATER LESSER_OR_EQUAL GREATER_OR_EQUAL
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
  | PRINT; e = expr
      { Print e }

  | VAR; dec = declare_block
      { dec }
  | VAR; dec = declare_var
      { dec }

  | RETURN; e = expr
      { Return e }

  | e = expr
      { ExprStmt e }

(* Expression grammar - builds up expression AST nodes *)
expr:
  (* outsourcing := foxes issues with precedence, as this was considered infix *)
  | name = IDENT; assign; e = expr
      { Assign (name, e) }

  (* assign binds *)
  | name = IDENT; BIND; op = bin_op; e = expr
      { Assign (name, BinOp (op, Var (name), e)) }

  | name = IDENT; BIND; INCREMENT; m = bin_op_mod
      { Assign (name, BinOp (Add (m), Var (name), Num 1.)) }
  | name = IDENT; BIND; DECREMENT; m = bin_op_mod
      { Assign (name, BinOp (Sub (m), Var (name), Num 1.)) }


  (* post assign binds *)
  (* for some mystical occult reason I'm too much of a quiche-eater to
     understand, in this case, the bin_op_mod cannot properly handle empty
     cases, so there needs to be some duplicity *)
  (* same problem here now, it just does not like starting with stuff that has
     precedence, except that unary +/- are fine...*)
  | EQUALS; BIND; name = IDENT; e = expr
      { PostAssign (name, e) }

  (** this still throws parsers errors though... *)
  (* | PLUS; BIND; name = IDENT; e = expr *)
  (*     { PostAssign (name, BinOp (Add NoMod, Var (name), e)) } *)

  | INCREMENT; BIND; name = IDENT
      { PostAssign (name, BinOp (Add NoMod, Var (name), Num 1.)) }
  | DECREMENT; BIND; name = IDENT
      { PostAssign (name, BinOp (Sub NoMod, Var (name), Num 1.)) }

  | INCREMENT; m = bin_op_mod; BIND; name = IDENT
      { PostAssign (name, BinOp (Add (m), Var (name), Num 1.)) }
  | DECREMENT; m = bin_op_mod; BIND; name = IDENT
      { PostAssign (name, BinOp (Sub (m), Var (name), Num 1.)) }


  | n = num
      { n }
  
  | x = IDENT
      { Var x }

  | x = SPECIAL_IDENT
      { SpecVar x }
  

  (* Parenthesized expression - just returns the inner expression *)
  | LPAREN; e = expr; RPAREN
      { e }

  | DO; blk = code_block
      { blk }

  (* we expect If to only have Blocks as it's values in codegen *)
  | IF; cond = code_block; BIND; t = code_block; BIND; f = code_block
      { If (cond, t, f) }
  | IF; cond = code_block; BIND; t = code_block
      { If (cond, t, Block ([], SpecVar "nil")) }

  | UNLESS; cond = code_block; BIND; t = code_block; BIND; f = code_block
      { If (UnOp(Not, cond), t, f) }
  | UNLESS; cond = code_block; BIND; t = code_block
      { If (UnOp(Not, cond), t, Block ([], SpecVar "nil")) }

  | LSQUARE; e = list(expr); RSQUARE
      { match e with
        | []         -> Block ([], SpecVar "nil")(* how to raise error again? *)
        | Var h :: t -> VarFunc (h, t)
        | h :: t     -> ValFunc (h, t)}



  (* | WHILE; cond = code_block; BIND; LSQUARE; dec = option(terminated(list(declare_var))); BIND; LSQUARE; body = list(stmt); RSQUARE; *)
  (*     { While (cond, (StmtList dec), Block (body, SpecVar "nil")) } *)
  | WHILE; cond = code_block; BIND; dec = declare_block; BIND; body = code_block
      { While (cond, dec, body) }
  | WHILE; cond = code_block; BIND; body = code_block
      { While (cond, StmtList [], body) }

  | UNTIL; cond = code_block; BIND; dec = declare_block; BIND; body = code_block
      { While (UnOp (Not, cond), dec, body) }
  | UNTIL; cond = code_block; BIND; body = code_block
      { While (UnOp (Not, cond), StmtList [], body) }

  | LAMBDA; args = arg_block; BIND; dec = declare_block; BIND; body = code_block
      { Lambda (args, dec, body) }
  | LAMBDA; args = arg_block; BIND; body = code_block
      { Lambda (args, StmtList [], body) }
  
  (* cannot be factored, because percedence *)
  | e1 = expr; PLUS; m=bin_op_mod ; e2 = expr
    { BinOp ((Add m), e1, e2) }

  | e1 = expr; MINUS; m=bin_op_mod ; e2 = expr
    { BinOp ((Sub m), e1, e2) }

  | e1 = expr; TIMES; m=bin_op_mod ; e2 = expr

  | e1 = expr; DIVIDE; m=bin_op_mod ; e2 = expr
    { BinOp ((Div m), e1, e2) }

  | e1 = expr; MODULO; m=bin_op_mod ; e2 = expr
    { BinOp ((Mod m), e1, e2) }

  | e1 = expr; WHOLE_DIVIDE; m=bin_op_mod ; e2 = expr
    { BinOp ((WholeDiv m), e1, e2) }

  | e1 = expr; EQUALS; e2 = expr            { BinOp (Equals, e1, e2) }
  | e1 = expr; LESSER; e2 = expr            { BinOp (Lesser, e1, e2) }
  | e1 = expr; GREATER; e2 = expr           { BinOp (Greater, e1, e2) }
  | e1 = expr; LESSER_OR_EQUAL; e2 = expr   { BinOp (LesserEq, e1, e2) }
  | e1 = expr; GREATER_OR_EQUAL; e2 = expr  { BinOp (GreaterEq, e1, e2) }
  | e1 = expr; NOT_EQUALS; e2 = expr        { BinOp (NotEquals, e1, e2) }
  | e1 = expr; AND; e2 = expr               { BinOp (And, e1, e2) }
  | e1 = expr; OR; e2 = expr                { BinOp (Or,  e1, e2) }
  | e1 = expr; XOR; e2 = expr               { BinOp (Xor, e1, e2) }



  (* Unary operations *)
  | NOT; e = expr
      { UnOp (Not, e) }

  | MINUS; e = expr %prec unary_minus
      { UnOp (Minus, e)  }
    
  (* to allow -++--foo, since it's fun*)
  | PLUS     ; e = expr %prec unary_plus
  | INCREMENT; e = expr %prec unary_plus
  | DECREMENT; e = expr %prec unary_plus
      { UnOp (Plus, e) }



  | cxr = CXR { Cxr cxr }
  | e1 = expr; CONS; e2 = expr
      { Cons (e1, e2) }

assign: | BIND; EQUALS {}
num:
  | MINUS; n = NUM
      { Num (-.n)}
  | n = NUM
      { Num n }

bin_op:
  | PLUS;         m = bin_op_mod { Add      m }
  | MINUS;        m = bin_op_mod { Sub      m }
  | TIMES;        m = bin_op_mod { Mul      m }
  | DIVIDE;       m = bin_op_mod { Div      m }
  | MODULO;       m = bin_op_mod { Mod      m }
  | WHOLE_DIVIDE; m = bin_op_mod { WholeDiv m }


bin_op_mod:
  | BIND; LESSER; BIND; e1 = expr; BIND; e2 = expr; BIND; GREATER
      { InRange (e1, e2) }

  | BIND; MODULO; BIND; LESSER; BIND; e1 = expr; BIND; e2 = expr; BIND; GREATER
      { LoopInRange (e1, e2) }

  | BIND; LESSER; BIND; e = expr
      { UpTo e }

  | BIND; GREATER; BIND; e = expr
      { DownTo e }

  | BIND; MODULO; BIND; e = expr
      { ModTo e }

  | BIND; n = num
      { UpTo n }

  | { NoMod }


(* `s = list(stmt); e = expr` failed me, so we're doing this then I guess *)
code_block:
  | LSQUARE; s = list(stmt); RSQUARE
      { match (List.rev s) with
          | ExprStmt(e) :: t -> Block (List.rev t, e)
          | x -> Block (List.rev x, SpecVar "nil")}

declare_block:
  | LSQUARE; s = nonempty_list(declare_var); RSQUARE
      { StmtList s }

declare_var:
  | name = IDENT; BIND; value = expr
      { Declare (name, value) }

  | name = IDENT
      { Declare (name, SpecVar "nil") }

arg_block:
  | LSQUARE; params = list(argument); RSQUARE
      { params }

argument:
  | name = IDENT
      { SimpleArg name }
