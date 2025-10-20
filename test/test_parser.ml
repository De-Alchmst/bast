open Bast_lib
open Ast

let parse_string str =
  let lexbuf = Lexing.from_string str in
  Parser.prog Lexer.tokenize lexbuf


let run_test name input expected =
  try
    let actual = parse_string input in
    if actual = expected then begin
      Printf.printf "✓ %s\n" name;
      true
    end else begin
      Printf.printf "✗ %s: AST mismatch\n" name;
      false
    end
  with
  | Lexer.LexError msg ->
      Printf.printf "✗ %s: Lexer error: %s\n" name msg;
      false
  | Parser.Error ->
      Printf.printf "✗ %s: Parser error\n" name;
      false
  | e ->
      Printf.printf "✗ %s: Unexpected error: %s\n" name (Printexc.to_string e);
      false


let tests = [
  ("simple assignment",
   "foo = 42",
   [Assign ("foo", Num 42.)]);
  
  ("variable assignment",
   "foo = bar",
   [Assign ("foo", Var "bar")]);
  
  ("addition",
   "foo = 1 + 2",
   [Assign ("foo", BinOp (Add, Num 1., Num 2.))]);
  
  ("precedence mul before add",
   "foo = 1 + 2 * 3",
   [Assign ("foo", BinOp (Add, Num 1., BinOp (Mul, Num 2., Num 3.)))]);
  
  ("parentheses",
   "foo = (1 + 2) * 3",
   [Assign ("foo", BinOp (Mul, BinOp (Add, Num 1., Num 2.), Num 3.))]);
  
  ("left associativity",
   "foo = 10 - 5 - 2",
   [Assign ("foo", BinOp (Sub, BinOp (Sub, Num 10., Num 5.), Num 2.))]);
  
  ("print statement",
   "print 42",
   [Print (Num 42.)]);
  
  ("print expression",
   "print foo + 5",
   [Print (BinOp (Add, Var "foo", Num 5.))]);
  
  ("multiple statements",
   "foo = 10\nbar = 20\nprint foo + bar",
   [
     Assign ("foo", Num 10.);
     Assign ("bar", Num 20.);
     Print (BinOp (Add, Var "foo", Var "bar"))
   ]);
  
  ("complex expression",
   "result = (foo + bar) * (baz - bax) / 2",
   [Assign ("result", 
     BinOp (Div,
       BinOp (Mul,
         BinOp (Add, Var "foo", Var "bar"),
         BinOp (Sub, Var "baz", Var "bax")),
       Num 2.))]);
  
  ("all operators",
   "foo = 1 + 2 - 3 * 4 / 5",
   [Assign ("foo",
     BinOp (Sub,
       BinOp (Add, Num 1., Num 2.),
       BinOp (Div, BinOp (Mul, Num 3., Num 4.), Num 5.)))]);

  ("ExprStmt",
   "5 + 7",
   [ExprStmt (BinOp (Add, Num 5., Num 7.))]);

  ("Nil block",
   "do [3+2 foo = 9]",
   [ExprStmt (Block
               ([ExprStmt (BinOp (Add, Num 3., Num 2.));
                 Assign ("foo", Num 9.)],
               SpecVar "nil"))]);

  ("Value block",
   "foo = blk [7]",
   [Assign ("foo", Block ([], Num 7.))]);

  ("VarFunc",
   "[foo bar 7]",
   [ExprStmt (VarFunc ("foo", [Var "bar"; Num 7.]))]);

  ("ValFunc",
   "[f+ bar 7]",
   [ExprStmt (ValFunc (SpecVar "f+", [Var "bar"; Num 7.]))]);
]


let () =
  Printf.printf "Running parser tests...\n\n";
  
  let results = List.map (fun (name, input, expected) ->
    run_test name input expected
  ) tests in
  
  let passed = List.filter (fun x -> x) results |> List.length in
  let total = List.length tests in
  
  Printf.printf "\n%d/%d tests passed\n" passed total;
  
  if passed = total then
    exit 0
  else
    exit 1
