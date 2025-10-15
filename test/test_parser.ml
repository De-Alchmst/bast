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
  (* Test 1: Simple integer literal assignment *)
  ("simple assignment",
   "x = 42",
   [Assign ("x", Num 42.)]);
  
  (* Test 2: Variable reference *)
  ("variable assignment",
   "y = x",
   [Assign ("y", Var "x")]);
  
  (* Test 3: Simple addition *)
  ("addition",
   "z = 1 + 2",
   [Assign ("z", BinOp (Add, Num 1., Num 2.))]);
  
  (* Test 4: Operator precedence - multiplication before addition *)
  ("precedence mul before add",
   "a = 1 + 2 * 3",
   [Assign ("a", BinOp (Add, Num 1., BinOp (Mul, Num 2., Num 3.)))]);
  
  (* Test 5: Parentheses override precedence *)
  ("parentheses",
   "b = (1 + 2) * 3",
   [Assign ("b", BinOp (Mul, BinOp (Add, Num 1., Num 2.), Num 3.))]);
  
  (* Test 6: Left associativity *)
  ("left associativity",
   "c = 10 - 5 - 2",
   [Assign ("c", BinOp (Sub, BinOp (Sub, Num 10., Num 5.), Num 2.))]);
  
  (* Test 7: Print statement *)
  ("print statement",
   "print 42",
   [Print (Num 42.)]);
  
  (* Test 8: Print expression *)
  ("print expression",
   "print x + 5",
   [Print (BinOp (Add, Var "x", Num 5.))]);
  
  (* Test 9: Multiple statements *)
  ("multiple statements",
   "x = 10\ny = 20\nprint x + y",
   [
     Assign ("x", Num 10.);
     Assign ("y", Num 20.);
     Print (BinOp (Add, Var "x", Var "y"))
   ]);
  
  (* Test 10: Complex expression *)
  ("complex expression",
   "result = (a + b) * (c - d) / 2",
   [Assign ("result", 
     BinOp (Div,
       BinOp (Mul,
         BinOp (Add, Var "a", Var "b"),
         BinOp (Sub, Var "c", Var "d")),
       Num 2.))]);
  
  (* Test 11: All operators *)
  ("all operators",
   "x = 1 + 2 - 3 * 4 / 5",
   [Assign ("x",
     BinOp (Sub,
       BinOp (Add, Num 1., Num 2.),
       BinOp (Div, BinOp (Mul, Num 3., Num 4.), Num 5.)))]);
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
