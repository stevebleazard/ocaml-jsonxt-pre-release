let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

open Printf

let print_token = function
  | `Null -> printf " `Null "
  | `Bool b -> printf "(`Bool %s)" (if b then "true" else "false")
  | `Int i -> printf "(`Int %d)" i
  | `Intlit s -> printf "(`Intlit %s)" s
  | `Float f -> printf "(`Float %g)" f
  | `Floatlit s -> printf "(`Floatlit %s)" s
  | `String s -> printf "(`String %s)" s
  | `Stringlit s -> printf "(`Stringlit %s)" s
  | `As -> printf " `As "
  | `Ae -> printf " `Ae "
  | `Os -> printf " `Os "
  | `Oe -> printf " `Oe "
  | `Ts -> printf " `Ts "
  | `Te -> printf " `Te "
  | `Vs -> printf " `Vs "
  | `Ve -> printf " `Ve "
  | `Name s -> printf "(`Name %s)" s
  | `Infinity -> printf " `Infinity "
  | `Neg_infinity -> printf " `Neg_infinity "
  | `Nan -> printf " `Nan "

let parsit contents =
  let stream = Jsonxt.Basic_stream.json_stream_of_string contents in
  let rec loop () =
    match Jsonxt.Basic_stream.decode_stream stream with
    | Ok None -> ()
    | Ok (Some tok) -> print_token tok; loop ()
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
      parsit contents
