let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

open Printf

let parsit_writer contents =
  let stream = Jsonxt.Extended_stream.json_stream_of_string contents in
  let printer = Jsonxt.Extended_stream.create_encoder_hum ~add_char:(printf "%c") ~add_string:(printf "%s") in
  let rec loop () =
    match Jsonxt.Extended_stream.decode_stream stream with
    | Ok None -> ()
    | Ok (Some tok) -> Jsonxt.Extended_stream.encode_stream_exn printer tok; loop ()
    | Error s -> printf "Error %s\n" s
  in
    loop ();
  printf "\n"

let () = 
  if Array.length Sys.argv < 2 then
    printf "expected filename\n"
  else
    let filename = Sys.argv.(1) in
    let contents = load_file filename in
      parsit_writer contents
