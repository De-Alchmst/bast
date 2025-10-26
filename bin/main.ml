(* main.ml - Main entry point for the compiler *)

open Bast_lib

let is_bast str =
  let name = String.lowercase_ascii str in
  let rec aux = function
    | [] -> false
    | suffix :: rest ->
      if String.ends_with ~suffix:suffix name then true else aux rest
  in aux [".bast"; ".bat"; ".☥"; ".𓋹"]

let compile_file input_filename output_filename =
  let input_channel = In_channel.open_text input_filename in
  let input_str = String.lowercase_ascii (In_channel.input_all input_channel) in
  In_channel.close input_channel;

  let lexbuf = Lexing.from_string input_str in
  
  (* Set the filename for error messages (optional but helpful) *)
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = input_filename };
  
  try
    let ast = Parser.prog Lexer.tokenize lexbuf in
    Moonbit_project.write_file_ast output_filename ast;
    0

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

let build () =
    Moonbit_project.run ()

(* Entry point *)
let () =
  (* Read from standard input and compile *)
  Moonbit_project.gen_skelet ();
  let registered_filenames = ref ["main.mbt"] in

  (* compile all BAST files in current directory and store their moonbit 
     counterparts to registered_filenames *)
  Sys.readdir "./" |> Array.iter (fun filename ->
    if is_bast filename then
      let out_filename = Encoding.output_filename filename in
        registered_filenames := filename :: !registered_filenames;
        let return_code = compile_file filename out_filename in
          if return_code != 0 then exit return_code);



  build ();
  exit 0
