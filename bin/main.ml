(* main.ml - Main entry point for the compiler *)

open Bast_comp_lib

let is_bast str =
  let name = String.lowercase_ascii str in
  let rec aux = function
    | [] -> false
    | suffix :: rest ->
      if String.ends_with ~suffix:suffix name then true else aux rest
  in aux [".bast"; ".bst"; ".☥"; ".𓋹"]


let compile_string input_filename output_filename input_str =

  let lexbuf = Lexing.from_string input_str in
  
  (* Set the filename for error messages (optional but helpful) *)
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = input_filename };
  
  try
    let ast = Parser.prog Lexer.next_token lexbuf in
    Moonbit_project.write_file_ast output_filename ast;
    0

  with
  (* Handle lexer errors *)
  | Lexer.LexError msg ->
      Printf.eprintf "Lexical error in file %s: %s\n" input_filename msg;
      1
  
  (* Handle parser errors - Menhir raises this *)
  | Parser.Error ->
      (* Get position information for better error messages *)
      let pos = Lexing.lexeme_start_p lexbuf in
      Printf.eprintf "Parse error at %s:%d:%d\n"
        input_filename
        pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol);
      1
  
  (* Catch any other unexpected errors *)
  | e ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      1

let compile_file input_filename output_filename =
  let input_str = String.lowercase_ascii @@ Files.read_file input_filename in
  compile_string input_filename output_filename input_str


let build () =
    Moonbit_project.run ()

(* Entry point *)
let () =
  Moonbit_conf.parse_config ();

  Moonbit_project.gen_skelet ();
  let registered_filenames = ref ["main.mbt"; "moon-lib.mbt"; "bast-lib.mbt"] in

  (* compile all BAST files in current directory and store their moonbit 
     counterparts to registered_filenames *)
  Sys.readdir "./" |> Array.iter (fun filename ->
    if is_bast filename then
      let out_filename = Encoding.output_filename filename in
        registered_filenames := out_filename :: !registered_filenames;

        let return_code = compile_file filename out_filename in
          if return_code != 0 then exit return_code);

  if Moonbit_project.version_changed then
  begin
    let return_code = compile_string "BAST lib" "bast-lib.mbt"
                                     (String.lowercase_ascii Bast_lib.src) in
      if return_code != 0 then exit return_code;
  end;

  (* remove untracked moonbit files *)
  Sys.readdir Moonbit_project.basedir |> Array.iter (fun filename ->
    if String.ends_with ~suffix:".mbt" filename then
      if not (List.mem filename !registered_filenames) then
        Sys.remove (Moonbit_project.basedir ^ filename));

  Moonbit_project.gen_extern ();

  build ();
  exit 0
