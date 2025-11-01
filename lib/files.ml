let mkdir path =
  if not (Sys.file_exists path) then Unix.mkdir path 0o777


let write_file_string name str =
  let oc = Out_channel.open_text name in
  Out_channel.output_string oc str;
  Out_channel.flush oc;
  Out_channel.close oc


let create_file_string name str =
  if not (Sys.file_exists name) then
    write_file_string name str


let read_file name =
  let ic = In_channel.open_text name in
  let str = In_channel.input_all ic in
  In_channel.close ic;
  str


(* https://stackoverflow.com/questions/56327912/how-to-remove-a-non-empty-directory-with-ocaml *)
let rec rmrf path = match Sys.is_directory path with
  | true ->
    Sys.readdir path |>
      Array.iter (fun name -> rmrf (Filename.concat path name));
    Unix.rmdir path
  | false -> Sys.remove path

