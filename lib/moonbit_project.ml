open Moonbit_conf
open Moonbit_codegen
open Ast

let basedir = "_BAST_work_dir/"
let version = "moonbit-t.43"
let version_file_name = basedir ^ "compiler.version"
let version_changed = 
  if Sys.file_exists version_file_name then
    if not (version = Files.read_file version_file_name) then true else false
  else true

let gen_moon_mod () =
  Files.create_file_string (basedir ^ "moon.mod.json") 
{|{
  "name": "bast-program"
}|}


let gen_moon_pkg () =
  Files.create_file_string (basedir ^ "moon.pkg.json")
  (Printf.sprintf
{|{
  "warn-list": "-1-2-3-4-5-6-7-8-9",
  "is-main": %s,
  "link": {
    "wasm": {
      %s
      %s
      %s
    }
  }
}|}
  (if !entrypoint = "" then "false" else "true") 

  (if !heap_start = "" then "" else
    "\"heap-start-address\": " ^ !heap_start ^ ",")

  (if !export_functions = [] then "" else
    Printf.sprintf "\"exports\": [%s],"
      (String.concat "," (!export_functions |> List.map (function
        | (exp_name, _) -> Printf.sprintf "\"%s\"" exp_name))))

  (if !import_memory = ("", "") then "" else
    match !import_memory with | (modul, name) ->
      Printf.sprintf "\"import-memory\": {\"module\":\"%s\", \"name\":\"%s\"}" 
        modul name))
      


let gen_moon_main () =
  let main_filename = basedir ^ "main.mbt" in
  if !entrypoint = "" then
    Files.rmrf main_filename
  else
    Files.create_file_string main_filename
    ((Encoding.encode_prefix !entrypoint)
    |> Printf.sprintf
{|fn main{
 let _=call_var_func(%s,[])
}|})


let string_of_type = function
  | NoType -> "Unit"
  | I32    -> "Int"
  | I64    -> "Int64"
  | U32    -> "UInt"
  | U64    -> "UInt64"
  | F32    -> "Float"
  | F64    -> "Double"

let string_of_func_args args =
  String.concat ", " (List.map (function
    | (str, typ) -> Printf.sprintf "%s: %s"
        (Encoding.encode_prefix str)
        (string_of_type typ))
  args)


let gen_external_functions_file () =
  Files.create_file_string (basedir ^ "extern.mbt")
  (String.concat "\n"
    [(String.concat "\n" ((!extern_functions
      |> (List.map (function
          | (ext_name, args, ret_type, extern_ident) ->
              Printf.sprintf "fn %s(%s) -> %s = \"%s\""
                ext_name
                (string_of_func_args args)
                (string_of_type ret_type)
          (String.concat "\" \"" extern_ident))))));

    (String.concat "\n" ((!export_functions
      |> (List.map (function
          | (exp_name, pref_name) ->
              Printf.sprintf "pub fn %s() -> Unit {ignore(call_var_func(%s, []))}"
                exp_name
                pref_name)))))])

let gen_moon_lib () =
  Files.create_file_string (basedir ^ "moon-lib.mbt") Moonbit_lib.src


let gen_version_file () =
  Files.create_file_string version_file_name version


let gen_skelet () =
  if version_changed then
  begin
    print_endline "re-creating build dir";
    Files.rmrf basedir;
  end;
  
  Files.mkdir  basedir;
  gen_version_file ();
  gen_moon_lib     ();
  gen_moon_mod     ();
  gen_moon_main    ()


let gen_extern () =
  gen_moon_pkg ();
  gen_external_functions_file ()


let file_of_ast ast = string_of_ast ast

let write_file_ast name ast =
  Files.write_file_string (basedir ^ name) (file_of_ast ast)


let build () =
  Sys.chdir basedir;
  (* So this is where moonbit got the idea... *)
  (* I hate you OCaml! *)
  ignore (Sys.command "moon build --release --target wasm --strip");
  if !entrypoint = "" then
    Sys.rename "target/wasm/release/build/build.output" "../out.wasm"
  else
    Sys.rename "target/wasm/release/build/bast-program.wasm" "../out.wasm";
  Sys.chdir ".."
  (* ignore (Sys.command "wasm-opt -Oz --zero-filled-memory --strip-producers --enable-sign-ext --enable-threads --enable-mutable-globals --enable-nontrapping-float-to-int --enable-simd --enable-bulk-memory --enable-bulk-memory-opt --enable-call-indirect-overlong --enable-exception-handling --enable-tail-call --enable-reference-types --enable-multivalue --enable-relaxed-simd --enable-extended-const --enable-strings --enable-multimemory --enable-stack-switching --enable-shared-everything --enable-fp16 --enable-custom-descriptors ./out.wasm -o out.wasm") *)

let run () =
  build ();
  (if !entrypoint <> "" then ignore (Sys.command "moonrun out.wasm"))

