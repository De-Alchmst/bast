(* lexer.mll - Lexical analyzer specification *)

(* This section contains OCaml code that will be copied directly into the
   generated lexer.ml file. We use it for helper functions and exceptions. *)
{
  open Parser
  
  exception LexError of string
}

let whitespace = [' ' '\t' '\r']
let newline = '\n'
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

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
  | "return"     { RETURN }
  | "func"       { FUNC }
  | "lambda" | "lamb" | "λ" { LAMBDA }
  | "do" | "blk" | "blck" | "block" { DO }

  | "println"     { SPECIAL_IDENT "println" }
  | "nil"   | "n" { SPECIAL_IDENT "nil" }
  | "true"  | "t" { SPECIAL_IDENT "true" }
  | "false" | "f" { SPECIAL_IDENT "false" }
  | 'f' ('+'|'-'|'*'|"//"|'/'|'%'
        |"!"|"&&"|"||"|"^^"|"<="|">="|"="|"<"|">")
      { SPECIAL_IDENT (Lexing.lexeme lexbuf) }
  | "f!=" | "f<>" { SPECIAL_IDENT "f!=" }

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
  | "car"   | "cdr"    { SPECIAL_IDENT (Lexing.lexeme lexbuf) }
  | 'c' ('a'|'d')+ 'r' { CXR (Lexing.lexeme lexbuf) }

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
  | '['            { LSQUARE }
  | ']'            { RSQUARE }
  | '{'            { LCURLY }
  | '}'            { RCURLY }

  | '\\'         { CONS }
  
  | eof          { EOF }
  
  (* If we encounter any other character, raise an error *)
  | _ as c       
      { 
        raise (LexError (Printf.sprintf "Unexpected character: %c" c)) 
      }
