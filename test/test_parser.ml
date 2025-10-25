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
  ("variable declaration",
  "var foo : 42 var [bar baz:foo]",
   [Declare ("foo", Num 42.);
    StmtList [
      Declare ("bar", SpecVar "nil");
      Declare ("baz", Var "foo")
    ]]);

  ("simple assignment",
   "foo := 42",
   [ExprStmt (Assign ("foo", Num 42.))]);
  
  ("variable assignment",
   "foo := bar",
   [ExprStmt (Assign ("foo", Var "bar"))]);
  
  ("addition",
   "foo := 1 + 2",
   [ExprStmt (Assign ("foo", BinOp (Add NoMod, Num 1., Num 2.)))]);
  
  ("precedence mul before add",
   "foo := 1 + 2 * 3",
   [ExprStmt (Assign ("foo", BinOp (Add NoMod, Num 1., BinOp (Mul NoMod, Num 2., Num 3.))))]);
  
  ("parentheses",
   "foo := (1 + 2) * 3",
   [ExprStmt (Assign ("foo", BinOp (Mul NoMod, BinOp (Add NoMod, Num 1., Num 2.), Num 3.)))]);
  
  ("left associativity",
   "foo := 10 - 5-2",
   [ExprStmt (Assign ("foo", BinOp (Sub NoMod, BinOp (Sub NoMod, Num 10., Num 5.), Num 2.)))]);
  
  ("print statement",
   "print 42",
   [Print (Num 42.)]);
  
  ("print expression",
   "print foo + 5",
   [Print (BinOp (Add NoMod, Var "foo", Num 5.))]);
  
  ("multiple statements",
   "foo := 10\nbar := 20\nprint foo + bar",
   [
     ExprStmt (Assign ("foo", Num 10.));
     ExprStmt (Assign ("bar", Num 20.));
     Print (BinOp (Add NoMod, Var "foo", Var "bar"))
   ]);
  
  ("complex expression",
   "result := (foo + bar) * (baz - bax) / 2",
   [ExprStmt (Assign ("result", 
     BinOp (Div NoMod,
       BinOp (Mul NoMod,
         BinOp (Add NoMod, Var "foo", Var "bar"),
         BinOp (Sub NoMod, Var "baz", Var "bax")),
       Num 2.)))]);
  
  ("all binary operators",
   "foo := 1 + 2 - 3 * 4 / 5 // 4 % 3",
   [ExprStmt (Assign ("foo",
     BinOp (Sub NoMod,
       BinOp (Add NoMod, Num 1., Num 2.),
       BinOp (Mod NoMod,
              BinOp (WholeDiv NoMod,
                     BinOp (Div NoMod,
                            BinOp (Mul NoMod, Num 3., Num 4.),
                            Num 5.),
                     Num 4.),
              Num 3.))))]);

  ("all of unary operators",
   "3 * +- foo",
   [ExprStmt (BinOp (Mul NoMod, Num 3., UnOp (Plus, UnOp (Minus, Var "foo"))))]);

  ("ExprStmt",
   "5 + 7",
   [ExprStmt (BinOp (Add NoMod, Num 5., Num 7.))]);

  ("Nil block",
   "do [3+2 print 69]",
   [ExprStmt (Block
               ([ExprStmt (BinOp (Add NoMod, Num 3., Num 2.));
                 Print (Num 69.)],
               SpecVar "nil"))]);

  ("Value block",
   "foo:=blk[7]",
   [ExprStmt (Assign ("foo", Block ([], Num 7.)))]);

  ("VarFunc",
   "[foo bar 7]",
   [ExprStmt (VarFunc ("foo", [Var "bar"; Num 7.]))]);

  ("ValFunc",
   "[f+ bar 7]",
   [ExprStmt (ValFunc (SpecVar "f+", [Var "bar"; Num 7.]))]);

  ("BinOp bind",
   "4 +:>:-2*2 0\nfoo -:3 7",
   [ExprStmt (BinOp (Add (DownTo (BinOp (Mul NoMod, Num (-2.), Num 2.))),
                         Num 4., Num 0.));
    ExprStmt (BinOp (Sub (UpTo (Num 3.)), Var "foo", Num 7.))]);

  ("More complex bin op",
   "3 +:<:6:9:> 420 -:%:<:42:666:> 7",
   [ExprStmt (BinOp (Sub (LoopInRange (Num 42., Num 666.)),
               (BinOp (Add (InRange (Num 6., Num 9.)),
                 (Num 3.),
                 (Num 420.))),
               (Num 7.)))]);

  ("Assign as expression",
   "foo := bar := baz",
   [ExprStmt (Assign ("foo", Assign ("bar", Var "baz")))]);

  ("Bind operator to var",
   "foo:+:3 7",
   [ExprStmt (Assign ("foo", (BinOp (Add (UpTo(Num 3.)), Var "foo", Num 7.))))]);

  ("Increment/decrement",
   "foo:dec + bar:++",
   [ExprStmt (BinOp (Add NoMod,
                      (Assign ("foo", (BinOp (Sub NoMod, Var "foo", Num 1.)))),
                      (Assign ("bar", (BinOp (Add NoMod, Var "bar", Num 1.))))))]);

  ("Post Assignment",
   "foo + ++:80:bar + baz   --:foo  ;  =:bar 3",
   [ExprStmt (BinOp (Add NoMod,
                      (BinOp (Add NoMod,
                       (Var "foo"),
                       (PostAssign ("bar", (BinOp (Add (UpTo (Num 80.)), Var "bar", Num 1.)))))),
                     (Var "baz")));
    ExprStmt (PostAssign ("foo", (BinOp (Sub NoMod, Var "foo", Num 1.))))]);
    (* ExprStmt (PostAssign ("bar", Num 3.))]); *)

  ("basic comparison operators",
   "3 >= 7 xor 4 = 2 and 6 < 9",
   [ExprStmt (BinOp (And,
     BinOp (Xor,
       BinOp (GreaterEq, Num 3., Num 7.),
       BinOp (Equals, Num 4., Num 2.)),
     BinOp (Lesser, Num 6., Num 9.)))]);

  ("basic bool operators",
   "foo := true && false || t ^^ f",
   [ExprStmt (Assign ("foo",
     BinOp (Xor,
       BinOp (Or,
         BinOp (And,
           SpecVar "true",
           SpecVar "false"),
         SpecVar "true"),
       SpecVar "false")))]);

  ("If statement (actually an expression)",
   "if [true]:[foo]:[unless [bar]:[4]]",
   [ExprStmt (If (Block ([], SpecVar "true"),
                  Block ([], Var "foo"),
                  Block ([],
                    If (UnOp (Not, Block ([], Var "bar")),
                        Block ([], Num 4.),
                        Block ([], SpecVar "nil")))))]);

  ("Cons cell",
  "1\\2\\3\\n",
  [ExprStmt (Cons (Num 1., Cons (Num 2., Cons (Num 3., SpecVar "nil"))))]);

  ("While loop",
   "while [cnd]:[foo bar]:[until [baz]:[[bax]]]",
   [ExprStmt (While (Block ([], Var "cnd"),
                    (StmtList [
                      Declare ("foo", SpecVar "nil");
                      Declare ("bar", SpecVar "nil")]),
                      Block ([], While (UnOp (Not, Block ([], Var "baz")),
                                        StmtList [],
                                        Block ([], ValFunc (Var "bax", []))))))]);

  ("lambdas with declaration",
   "lamb [a1]:[v1:42]:[a1 + v1]",
   [ExprStmt (Lambda ([SimpleArg "a1"],
                       (StmtList [Declare ("v1", Num 42.)]),
                       (Block ([] ,BinOp (Add NoMod, Var "a1", Var "v1")))))]);

  ("lambdas without declaration",
   "lamb [a1 a2]:[a1 + a2]",
   [ExprStmt (Lambda ([SimpleArg "a1"; SimpleArg "a2"],
                       (StmtList []),
                       (Block ([] ,BinOp (Add NoMod, Var "a1", Var "a2")))))]);

  ("function",
   "func [foo a1]:[a1 * 2 a1]",
   [Declare ("foo", Lambda ([SimpleArg "a1"],
                            (StmtList []),
                            (Block
                              ([ExprStmt (BinOp (Mul NoMod, Var "a1", Num 2.))],
                               Var "a1"))))]);
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
