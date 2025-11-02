let conf_filename = "bast.conf"
let entrypoint = ref ""

let parse_config () =
  if Sys.file_exists conf_filename then
    Files.read_file_lines conf_filename
    |> List.iter (fun line ->
      match String.split_on_char ':' line |> List.map String.trim with
        | ["entry"; value] ->
            entrypoint := value
        | _ -> failwith @@ "cannot parse config : " ^ line)
