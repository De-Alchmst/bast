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
