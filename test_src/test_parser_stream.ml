let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

open Printf

module Printer = struct
  type json_stream =
      [
      | `Null | `Bool of bool | `Int of int | `Intlit of string | `Float of float | `Floatlit of string
      | `String of string | `Stringlit of string | `As | `Ae | `Os | `Oe | `Ts | `Te | `Vs | `Ve
      | `Name of string | `Infinity | `Neg_infinity | `Nan
      ]
  type t = (json_stream -> unit) Stack.t
  
  let print t tok = 
    let rec print_token t tok =
      match tok with
      | `Null -> printf "null"
      | `Bool b -> printf "%s" (if b then "true" else "false")
      | `Int i -> printf "%d" i
      | `Intlit s -> printf "\"%s\"" s
      | `Float f -> printf "%g" f
      | `Floatlit s -> printf "\"%s\"" s
      | `String s -> printf "\"%s\"" s
      | `Stringlit s -> printf "\"%s\"" s
      | `As -> printf "["; Stack.push (print_list_start t) t
      | `Ae -> printf "]"
      | `Os -> printf "{"; Stack.push (print_object_name t) t
      | `Oe -> printf "}"
      | `Ts -> printf "("; Stack.push (print_tuple_start t) t
      | `Te -> printf ")"
      | `Vs -> printf "<"; Stack.push (print_variant_start t) t
      | `Ve -> printf ">"
      | `Name s -> printf "\"%s\": " s
      | `Infinity -> printf "inf"
      | `Neg_infinity -> printf "-inf"
      | `Nan -> printf "nan"
    and print_list_start t tok =
      match tok with
      | `Ae -> printf "]"
      | _ -> let () = Stack.push (print_list_next t) t in print_token t tok
    and print_list_next t tok =
      match tok with
      | `Ae -> printf "]"
      | _ -> let () = Stack.push (print_list_next t) t in printf ", "; print_token t tok
    and print_object_name t tok =
      match tok with
      | `Oe -> printf "}"
      | `Name s -> printf "\"%s\":" s; Stack.push (print_object_value t) t
      | _ -> printf "!Syntax Error!"
    and print_object_name_next t tok =
      match tok with
      | `Oe -> printf "}"
      | `Name s -> printf ", \"%s\":" s; Stack.push (print_object_value t) t
      | _ -> printf "!Syntax Error!"
    and print_object_value t tok =
      match tok with
      | `Oe -> printf "!Syntax Error!"
      | _ -> Stack.push (print_object_name_next t) t; print_token t tok
    and print_tuple_start t tok =
      match tok with
      | `Te -> printf ")"
      | _ -> let () = Stack.push (print_tuple_next t) t in print_token t tok
    and print_tuple_next t tok =
      match tok with
      | `Te -> printf ")"
      | _ -> let () = Stack.push (print_tuple_next t) t in printf ", "; print_token t tok
    and print_variant_start t tok =
      match tok with
      | `Name s -> printf "\"%s\"" s; Stack.push (print_variant_value t) t
      | _ -> printf "!Syntax Error!"
    and print_variant_value t tok =
      match tok with
      | `Ve -> printf ">"
      | _ -> let () = Stack.push (print_variant_end t) t in printf ": "; print_token t tok
    and print_variant_end _t tok =
      match tok with
      | `Ve -> printf ">"
      | _ -> printf "!Syntax Error!"
    in
    let f = if Stack.is_empty t then print_token t else Stack.pop t in
      f tok

  let create = Stack.create

end

let parsit contents =
  let stream = Jsonxt.Extended_stream.json_stream_of_string contents in
  let printer = Printer.create () in
  let rec loop () =
    match Jsonxt.Extended_stream.decode_stream stream with
    | Ok None -> ()
    | Ok (Some tok) -> Printer.print printer tok; loop ()
    | Error s -> printf "Error %s\n" s
  in
    printf "Local\n";
    loop ();
  printf "\n"

let parsit_writer contents =
  let stream = Jsonxt.Extended_stream.json_stream_of_string contents in
  let printer = Jsonxt.Extended_stream.create_encoder' ~add_char:(printf "%c") ~add_string:(printf "%s") ~incr:0 ~eol:"" in
  let rec loop () =
    match Jsonxt.Extended_stream.decode_stream stream with
    | Ok None -> ()
    | Ok (Some tok) -> Jsonxt.Extended_stream.json_stream_encode_exn printer tok; loop ()
    | Error s -> printf "Error %s\n" s
  in
    printf "WRITER\n";
    loop ();
  printf "\n"

let () = 
  if Array.length Sys.argv < 2 then
    printf "expected filename\n"
  else
    let filename = Sys.argv.(1) in
    let contents = load_file filename in
      parsit contents;
      parsit_writer contents
