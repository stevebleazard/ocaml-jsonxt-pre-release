open Tokens

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let lex_and_print read lexbuf =
  let rec loop () =
    match read lexbuf with
    | FLOAT f ->  Printf.printf "Float [%g]\n" f; loop ()
    | INT i ->  Printf.printf "Int [%d]\n" i; loop ()
    | LARGEINT s ->  Printf.printf "Largeint [%s]\n" s; loop ()
    | STRING s -> Printf.printf "String [%s]\n" s; loop ()
    | BOOL b ->  Printf.printf "Bool [%s]\n" (if b then "true" else "false"); loop ()
    | NULL ->  Printf.printf "Null\n"; loop ()
    | AS ->  Printf.printf "A_start\n"; loop ()
    | AE ->  Printf.printf "A_end\n"; loop ()
    | OS ->  Printf.printf "O_start\n"; loop ()
    | OE ->  Printf.printf "O_end\n"; loop ()
    | COLON ->  Printf.printf "Colon\n"; loop ()
    | COMMA ->  Printf.printf "Comma\n"; loop ()
    | INFINITY ->  Printf.printf "Infinity\n"; loop ()
    | NEGINFINITY ->  Printf.printf "Neg_infinity\n"; loop ()
    | NAN ->  Printf.printf "Nan\n"; loop ()
    | EOF -> Printf.printf "EOF\n"
    | COMPLIANCE_ERROR err -> Printf.printf "COMPLIANCE_ERROR: %s\n" err; loop ()
    | LEX_ERROR err -> Printf.printf "COMPLIANCE_ERROR: %s\n" err; loop ()
  in
    loop ()

open Printf

let print_json_value json = 
  let rec fmt value =
    match value with
    | `Assoc o -> printf "{"; print_json_assoc o; printf "}"
    | `List l -> printf "["; print_json_list l; printf "]"
    | `Null -> printf "Null "
    | `Bool b -> printf "%s " (if b then "true" else "false")
    | `Int i -> printf "%d " i
    | `Intlit s -> printf "%s " s
    | `Float f -> printf "%g" f
    | `Floatlit s -> printf "%s " s
    | `String s -> printf "%s " s
    | `Stringlit s -> printf "%s " s
    | `Tuple t -> printf "tuple "
    | `Variant t -> printf "variant "
  and print_json_assoc o = List.iter print_pair o
  and print_pair (k, v) = printf "%s : " k; fmt v; printf ","
  and print_json_list l = List.iter (fun v -> fmt v; printf ",") l
  in
  fmt json

module IO = struct
  type 'a t = 'a
  
  let return v = v
  let (>>=) a f = f a
end

module Basic_lexxer = Compliant_lexxer.Make(Json_parse_types.Basic)
module Basic_parser_monad = Parser_monad.Make(Json_parse_types.Basic) (IO)
module Basic_parser = Parser.Make(Json_parse_types.Basic)

let parsit filename contents =
  let lexbuf = Lexing.from_string contents in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let open IO in
  let reader () = return (Basic_lexxer.read lexbuf) in
  Basic_parser.lax ~reader
  >>= function
    | Ok None -> Printf.printf "(*None*)\n"
    | Ok (Some json) -> print_json_value json; printf "\n"
    | Error s ->
      let loc = Compliant_lexxer.error_pos_msg lexbuf in
      printf "%s at %s\n" s loc

let testit filename contents =
  let lexbuf = Lexing.from_string contents in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let reader () = Basic_lexxer.read lexbuf in
  match Basic_parser.lax ~reader with
  | Ok None -> ()
  | Ok (Some json) -> ()
  | Error s ->
    let loc = Compliant_lexxer.error_pos_msg lexbuf in
      printf "%s at %s\n" s loc

let testit2 filename contents =
  let lexbuf = Lexing.from_string contents in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let reader () = Basic_lexxer.read lexbuf in
  match Basic_parser_monad.lax ~reader with
  | Ok None -> ()
  | Ok (Some json) -> ()
  | Error s ->
    let loc = Compliant_lexxer.error_pos_msg lexbuf in
      printf "%s at %s\n" s loc

let test_run n testf =
  for i = 1 to n do
    testf () |> ignore
  done

module Yj = struct
  open Yojson

  let read contents = Raw.from_string contents

  let benchit filename = 
    let contents = load_file filename in
    (fun () -> ignore (read contents))

  let testit _filename contents =
    Printf.printf "%s\n%!" _filename;
    (fun () -> ignore (read contents))

  let dumpit _filename contents =
    (fun () -> read contents |> Raw.to_string |> Printf.printf "%s\n")
end

(*
let () = 
  if Array.length Sys.argv < 2 then
    printf "expected filename\n"
  else
    let filename = Sys.argv.(1) in
    let contents = load_file filename in
    (* let testf = Yj.testit filename contents in *)
    let testf = (fun () -> testit filename contents) in
      test_run 100 testf
    (*
      parsit filename contents
      parsit "../test.json"
      lexit "../test.json"
    *)
*)

(* module Json_basic = Jsonxt_monad.Make(Json_parse_types.Basic) *)

open Core
open Core_bench.Std

let lex_and_discard reader =
  let rec loop () =
    match reader () with
    | Tokens.EOF -> ()
    | LEX_ERROR err -> Printf.printf "COMPLIANCE_ERROR: %s\n" err; loop ()
    | _ -> loop ()
  in
  loop ()

let testit_lex filename contents =
  let lexbuf = Lexing.from_string contents in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let reader () = Basic_lexxer.read lexbuf in
    lex_and_discard reader


let benchit filename = 
  let contents = load_file filename in
  (fun () -> ignore(testit filename contents))

let benchit2 filename = 
  let contents = load_file filename in
  (fun () -> ignore(testit2 filename contents))

let benchit_lex filename = 
  let contents = load_file filename in
  (fun () -> ignore(testit_lex filename contents))

let filename = "../test.json.10000"
let testxt = benchit filename
let testxtm = benchit2 filename
let testxt_lex = benchit_lex filename
let testyj = Yj.benchit filename

let () = Command.run (Bench.make_command [
    Bench.Test.create ~name:"lexxer" testxt_lex
  ; Bench.Test.create ~name:"parser" testxt
  (* ; Bench.Test.create ~name:"parserm" testxtm *)
  ; Bench.Test.create ~name:"yojson" testyj
  ])

(*
*)
