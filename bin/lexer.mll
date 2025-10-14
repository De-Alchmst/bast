(* lexer.mll - Lexical analyzer specification *)

(* This section contains OCaml code that will be copied directly into the
   generated lexer.ml file. We use it for helper functions and exceptions. *)
{
  open Parser  (* We need the token types defined in parser.mly *)
  
  (* Exception for when we encounter a character we don't recognize *)
  exception LexError of string
}

(* Named regular expressions - these make the rules more readable *)
let whitespace = [' ' '\t' '\r']  (* Spaces, tabs, carriage returns *)
let newline = '\n'                 (* Line breaks *)
let digit = ['0'-'9']              (* Digits 0-9 *)
let letter = ['a'-'z' 'A'-'Z']     (* Letters a-z, A-Z *)

(* The main lexing rule. This is a function that takes a lexbuf and returns tokens.
   'parse' is a keyword that means we're pattern matching on the input characters. *)
rule token = parse
  (* Skip whitespace by recursively calling token again *)
  | whitespace+  { token lexbuf }
  
  (* Skip newlines but also track line numbers for error reporting *)
  | newline      { Lexing.new_line lexbuf; token lexbuf }
  
  (* Keywords - must come before identifiers! *)
  | "print"      { PRINT }
  
  (* Identifiers (variable names) - must start with letter, can contain letters/digits *)
  | letter (letter | digit | '_')*  
      { 
        (* Lexing.lexeme extracts the actual text that matched *)
        IDENT (Lexing.lexeme lexbuf) 
      }
  
  (* Integer literals - one or more digits *)
  | digit+       
      { 
        (* Extract the matched text and convert to integer *)
        INT (int_of_string (Lexing.lexeme lexbuf)) 
      }
  
  (* Operators and punctuation - single character matches *)
  | '+'          { PLUS }
  | '-'          { MINUS }
  | '*'          { TIMES }
  | '/'          { DIVIDE }
  | '='          { EQUALS }
  | '('          { LPAREN }
  | ')'          { RPAREN }
  
  (* End of file *)
  | eof          { EOF }
  
  (* If we encounter any other character, raise an error *)
  | _ as c       
      { 
        raise (LexError (Printf.sprintf "Unexpected character: %c" c)) 
      }