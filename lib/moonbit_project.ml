open Moonbit_codegen
let basedir = "_BAST_work_dir/"
let version = "moonbit-t.1"
let version_file_name = basedir ^ "compiler.version"

let gen_moon_mod () =
  Files.create_file_string (basedir ^ "moon.mod.json") 
{|{
  "name": "bast-program"
}|}


let gen_moon_pkg () =
  Files.create_file_string (basedir ^ "moon.pkg.json")
{|{
  "warn-list": "-1-2-3-4-5-6-7-8-9",
  "is-main": true
}|}


let gen_moon_main () =
  Files.create_file_string (basedir ^ "main.mbt")
{|fn main{
 let _=call_var_func(_bast_main,[])
}|}


let gen_moon_lib () =
  Files.create_file_string (basedir ^ "bast-lib.mbt") Moonbit_lib.src


let gen_version_file () =
  Files.create_file_string version_file_name version


let gen_skelet () =
  if Sys.file_exists version_file_name then
    if not (version = Files.read_file version_file_name) then
      print_endline "re-creating build dir";
      Files.rmrf basedir;
  
  Files.mkdir  basedir;
  gen_version_file ();
  gen_moon_lib     ();
  gen_moon_mod     ();
  gen_moon_pkg     ();
  gen_moon_main    ()


let file_of_ast ast = string_of_ast ast

let write_file_ast name ast =
  Files.write_file_string (basedir ^ name) (file_of_ast ast)


let build () =
  Sys.chdir basedir;
  (* So this is where moonbit got the idea... *)
  (* I hate you OCaml! *)
  ignore (Sys.command "moon build --release --target wasm --strip");
  Sys.rename "target/wasm/release/build/bast-program.wasm" "../out.wasm";
  Sys.chdir ".."
  (* ignore (Sys.command "wasm-opt -Oz --zero-filled-memory --strip-producers --enable-sign-ext --enable-threads --enable-mutable-globals --enable-nontrapping-float-to-int --enable-simd --enable-bulk-memory --enable-bulk-memory-opt --enable-call-indirect-overlong --enable-exception-handling --enable-tail-call --enable-reference-types --enable-multivalue --enable-relaxed-simd --enable-extended-const --enable-strings --enable-multimemory --enable-stack-switching --enable-shared-everything --enable-fp16 --enable-custom-descriptors ./out.wasm -o out.wasm") *)

let run () =
  build ();
  ignore (Sys.command "moonrun out.wasm")
  (* ignore (Sys.command "ls -lh out.wasm") *)

