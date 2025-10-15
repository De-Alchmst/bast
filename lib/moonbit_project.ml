open Moonbit_codegen
let basedir = "_BAST_work_dir/"

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


let gen_moon_lib () =
  Files.create_file_string (basedir ^ "bast-lib.mbt") Moonbit_lib.src


let gen_skelet () =
  Files.mkdir  basedir;
  gen_moon_lib ();
  gen_moon_mod ();
  gen_moon_pkg ()



let file_of_ast ast =
{|fn main {
|}
^ string_of_ast ast ^
{|
}|}

let write_file_ast name ast =
  Files.write_file_string (basedir ^ name) (file_of_ast ast)


let build () =
  Sys.chdir basedir;
  (* So this is where moonbit got the idea... *)
  (* I hate you OCaml! *)
  ignore (Sys.command "moon build --release");
  Sys.rename "target/wasm-gc/release/build/bast-program.wasm" "../out.wasm";
  Sys.chdir ".."

let run () =
  build ();
  ignore (Sys.command "moonrun out.wasm");

