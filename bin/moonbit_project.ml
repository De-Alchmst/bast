open Moonbit_codegen
let basedir = "_BAST_work_dir/"

let gen_moon_mod () =
  Files.create_file_string (basedir ^ "moon.mod.json") 
{|{
  "name": "bast program"
}|}


let gen_moon_pkg () =
  Files.create_file_string (basedir ^ "moon.pkg.json")
{|{
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
