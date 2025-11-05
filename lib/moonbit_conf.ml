open Ast

let conf_filename = "bast.conf"
let entrypoint = ref ""
let heap_start = ref ""
let import_memory = ref ("", "")
let export_functions : (string * string) list ref = ref []
let extern_functions
  : (string * (string * func_type) list * func_type * string list) list ref
  = ref []

let parse_config () =
  if Sys.file_exists conf_filename then
    Files.read_file_lines conf_filename
    |> List.iter (fun line ->
      match String.split_on_char ':' line |> List.map String.trim with
        | ["entry"; value] -> entrypoint := value
        | ["heap-start"; value] -> heap_start := value
        | ["import-mem"; modul; name] -> import_memory := (modul, name)
        | _ -> failwith @@ "cannot parse config: " ^ line)
