(* main.ml - Main entry point for the compiler *)

open Bast_lib

let compile input_channel =
  (* Create a lexer buffer from the input channel - defined outside try
     so it's accessible in the error handlers *)
  let input_str = String.lowercase_ascii (In_channel.input_all input_channel) in
  let lexbuf = Lexing.from_string input_str in
  
  (* Set the filename for error messages (optional but helpful) *)
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "input" };
  
  try
    let ast = Parser.prog Lexer.tokenize lexbuf in
    Moonbit_project.gen_skelet ();
    Moonbit_project.write_file_ast "main.mbt" ast;
    Moonbit_project.run ();
    0 (* Return success *)
    
  with
  (* Handle lexer errors *)
  | Lexer.LexError msg ->
      Printf.eprintf "Lexical error: %s\n" msg;
      1
  
  (* Handle parser errors - Menhir raises this *)
  | Parser.Error ->
      (* Get position information for better error messages *)
      let pos = Lexing.lexeme_start_p lexbuf in
      Printf.eprintf "Parse error at line %d, column %d\n"
        pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol);
      1
  
  (* Catch any other unexpected errors *)
  | e ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      1

(* Entry point *)
let () =
  (* Read from standard input and compile *)
  let exit_code = compile stdin in
  exit exit_code
