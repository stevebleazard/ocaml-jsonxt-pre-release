let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

open Printf

let parsit contents =
  match Jsonxt.Basic.json_of_string contents with
  | Ok json -> let s = Jsonxt.Basic.to_string json in printf "%s\n" s
  | Error s -> printf "ERROR %s\n" s

let parsit_file_error_info filename =
  match Jsonxt.Basic.json_of_file_error_info filename with
  | Ok json -> let s = Jsonxt.Basic.to_string json in printf "ERR INFO: %s\n" s
  | Error { Jsonxt.Error_info.line; start_char; end_char; msg } ->
    printf "ERROR line %d, chars %d-%d: %s\n" line start_char end_char msg

let parse_stream contents =
  let stream = Jsonxt.Basic.stream_from_string contents in
  Stream.iter (fun json -> let s = Jsonxt.Basic.to_string json in printf "STREAM:\n%s\n" s) stream

(*
let parse_stream_file filename =
  let ic = open_in filename in
  let stream = Jsonxt.Basic.stream_from_channel ic in
  Stream.iter (fun json -> let s = Jsonxt.Basic.to_string json in printf "STREAM:\n%s\n" s) stream
*)
let parse_stream_file filename =
  let stream = Jsonxt.Basic.stream_from_file filename in
  Stream.iter (fun json -> let s = Jsonxt.Basic.to_string json in printf "STREAM:\n%s\n" s) stream

let parse_function filename =
  let ic = open_in filename in
  printf "json_of_function\n";
  match Jsonxt.Basic.json_of_function (fun buf len -> input ic buf 0 len) with
  (* | Ok json -> let s = Jsonxt.Basic.to_string json in printf "%s\n" s *)
  | Ok json ->
    let d = Jsonxt.Utilities.json_to_string_repr json in
    let s = Jsonxt.Utilities.json_to_string json in
    printf "%s\n%s\n" d s
  | Error s -> printf "ERROR %s\n" s

let () =
  if Array.length Sys.argv < 2 then
    printf "expected filename\n"
  else
    let filename = Sys.argv.(1) in
    let contents = load_file filename in
      parsit contents;
      parsit_file_error_info filename;
      parse_stream_file filename;
      parse_function filename
