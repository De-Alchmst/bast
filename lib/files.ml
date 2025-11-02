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
  In_channel.with_open_text name In_channel.input_all

let read_file_lines name =
  In_channel.with_open_text name In_channel.input_lines


(* https://stackoverflow.com/questions/56327912/how-to-remove-a-non-empty-directory-with-ocaml *)
let rmrf path =
  if Sys.file_exists path then
    let rec aux path =
      match Sys.is_directory path with
      | true ->
        Sys.readdir path |>
          Array.iter (fun name -> aux (Filename.concat path name));
        Unix.rmdir path
      | false -> Sys.remove path
    in aux path

