(* lexer.mll - Lexical analyzer specification *)

(* This section contains OCaml code that will be copied directly into the
   generated lexer.ml file. We use it for helper functions and exceptions. *)
{
  open Parser
  
  exception LexError of string

  let token_queue = Queue.create ()
  let queue_token tok = Queue.add tok token_queue

  let closing_block_stack = Stack.create ()

  let new_block () = Stack.push 0 closing_block_stack

  let add_to_block () =
    if Stack.is_empty closing_block_stack then new_block ();
    Stack.push ((Stack.pop closing_block_stack) + 1) closing_block_stack

  let close_block () =
    if not (Stack.is_empty closing_block_stack) then
      let count = Stack.pop closing_block_stack in
      for _ = 1 to count do
        queue_token RSQUARE
      done

  (* another block after tokenize *)
}



let whitespace = [' ' '\t' '\r']
let newline = '\n'
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let ad = ['a' 'd']

(* The main lexing rule. This is a function that takes a lexbuf and returns tokens.
   'parse' is a keyword that means we're pattern matching on the input characters. *)
rule tokenize = parse
  | whitespace+  { tokenize lexbuf }
  | newline      { Lexing.new_line lexbuf; tokenize lexbuf }
  | "var"        { VAR }
  | "if"         { IF }
  | "unless"     { UNLESS }
  | "while"      { WHILE }
  | "until"      { UNTIL }
  | "do-while"   { DO_WHILE }
  | "do-until"   { DO_UNTIL }
  | "loop"       { LOOP }
  | "for"        { FOR }
  | "return"     { RETURN }
  | "func"       { FUNC }
  | "list"       { LIST }
  | "read" | "r" { READ }
  | "write" | "w" { WRITE }
  | "cond" | "con" { COND }
  | "lambda" | "lamb" | "λ" { LAMBDA }
  | "do" | "blk" | "blck" | "block" { DO }

  | "println"     { SPECIAL_IDENT "println" }
  | "nil"   | "n" { SPECIAL_IDENT "nil" }
  | "true"  | "t" { SPECIAL_IDENT "true" }
  | "false" | "f" { SPECIAL_IDENT "false" }
  | 'f' ('+'|'-'|'*'|"//"|'/'|'%'
        |'!'|"&&"|"||"|"^^"|"<="|">="|'='|'<'|'>')
      { SPECIAL_IDENT (Lexing.lexeme lexbuf) }
  | "f!=" | "f<>" { SPECIAL_IDENT "f!=" }

  | "+>"           { add_to_block (); LSQUARE }

  | "<-"           { ARROW_LEFT }
  | "->"           { ARROW_RIGHT }
  | "<="           { FAT_ARROW_LEFT }
  | "=>"           { FAT_ARROW_RIGHT }

  | "inc" | "++"   { INCREMENT }
  | "dec" | "--"   { DECREMENT }
  | "not" | '!'    { NOT }
  | "and" | "&&"   { AND }
  | "or"  | "||"   { OR }
  | "xor" | "^^"   { XOR }
  | "!="  | "<>"   { NOT_EQUALS }
  | "<="           { LESSER_OR_EQUAL }
  | ">="           { GREATER_OR_EQUAL }

  | "cons"  | "f\\"    { SPECIAL_IDENT "cons" }
  | 'c' ad+ 'r'        { CXR (Lexing.lexeme lexbuf) }
  | 'r' ad+ 'c'        { CXR (Encoding.string_rev (Lexing.lexeme lexbuf)) }

  | "nil?" | "num?" | "atom?" | "bool?" | "func?" | "cons?" | "list?"
  | "Array?" | "String?"
      { SPECIAL_IDENT (Lexing.lexeme lexbuf) }

  | letter (letter | '-' | digit)+
      { IDENT (Lexing.lexeme lexbuf) }
  
  | digit+ '.'? (digit)*
      { 
        (* Extract the matched text and convert to integer *)
        NUM (float_of_string (Lexing.lexeme lexbuf)) 
      }


  (* comments *)
  | ';' ([^'\n'])+ { tokenize lexbuf }
  | ':'            { BIND }
  | '|'            { PIPE }
  | '`'            { NEGATE }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | "//"           { WHOLE_DIVIDE }
  | '/'            { DIVIDE }
  | '%'            { MODULO }
  | '>'            { GREATER }
  | '<'            { LESSER }
  | '='            { EQUALS }
  | '!'            { NOT }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '['            { new_block   (); LSQUARE }
  | ']'            { close_block (); RSQUARE }
  | '{'            { LCURLY }
  | '}'            { RCURLY }

  | '\\'           { CONS }
  
  | eof            { 
      if not (Stack.is_empty closing_block_stack) then
        add_to_block ();
        while not (Stack.is_empty closing_block_stack) do
          close_block ()
        done;
    EOF }
  
  (* If we encounter any other character, raise an error *)
  | _ as c       
      { 
        raise (LexError (Printf.sprintf "Unexpected character: %c" c)) 
}


{
  let next_token lexbuf =
    if Queue.is_empty token_queue then
      tokenize lexbuf
    else
      Queue.take token_queue
}
