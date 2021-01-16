let die msg = 
  Printf.fprintf stderr "\nERROR: %s\n" msg;
  exit 255

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s
