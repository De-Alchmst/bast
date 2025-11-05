let sanitize str = Str.global_replace  (Str.regexp "-") "_" str

let encode_prefix str = "_bast_" ^ sanitize str 
let encode_external_prefix str = "_externbast_" ^ sanitize str 
(* let encode_export_prefix str = "_exportbast_" ^ sanitize str *) 
let encode_export_prefix str = sanitize str 

let strip_ext filename =
  try
    let idx = String.rindex filename '.' in
    String.sub filename 0 idx
  with _ -> filename

(* keep extension so tahat both file.bast and file.bst can coexist *)
let output_filename name =
  "_bast_" ^ name ^ ".mbt"

let string_rev str =
  let rec aux n acc =
    if n >= String.length str
    then acc
    else aux (n+1) ((String.make 1 str.[n]) :: acc)
  in
  aux 0 [] |> String.concat ""
