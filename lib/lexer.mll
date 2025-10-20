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
  | "print"      { PRINT }
  | "var"        { VAR }
  | "return"     { RETURN }
  | "do" | "blk" | "blck" | "block" { DO }

  | "nil" | "n"  { SPECIAL_IDENT "nil" }
  | 'f' ('+'|'-'|'*'|"//"|'/'|'%') { SPECIAL_IDENT (Lexing.lexeme lexbuf) }

  | '-'* letter (letter | '-' | digit)+
      { IDENT (Lexing.lexeme lexbuf) }
  
  (* Integer literals - one or more digits *)
  | digit+ '.'? (digit)*
      { 
        (* Extract the matched text and convert to integer *)
        NUM (float_of_string (Lexing.lexeme lexbuf)) 
      }

  (* comments *)
  | ';' ([^'\n'])+ { tokenize lexbuf }

  | ':'          { BIND }

  | '+'          { PLUS }
  | '-'          { MINUS }
  | '*'          { TIMES }
  | "//"         { WHOLE_DIVIDE }
  | '/'          { DIVIDE }
  | '%'          { MODULO }
  | '='          { EQUALS }
  | '('          { LPAREN }
  | ')'          { RPAREN }
  | '['          { LSQUARE }
  | ']'          { RSQUARE }
  | '{'          { LCURLY }
  | '}'          { RCURLY }

  | '\\'         { CONS }
  
  | eof          { EOF }
  
  (* If we encounter any other character, raise an error *)
  | _ as c       
      { 
        raise (LexError (Printf.sprintf "Unexpected character: %c" c)) 
      }
