let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (Bytes.to_string s)


module Yj = struct
  open Yojson

  let read contents = Basic.from_string contents

  let dumpit contents = read contents |> Basic.to_string |> Printf.printf "%s\n"
end

let () = 
  if Array.length Sys.argv < 2 then
    Printf.printf "expected filename\n"
  else
    let filename = Sys.argv.(1) in
    let contents = load_file filename in
      Yj.dumpit contents
