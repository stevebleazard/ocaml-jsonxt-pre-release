let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let lex_and_print lexbuf =
  let rec loop () =
    match Lexxer.read lexbuf with
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
    (*
    | _ ->  loop ()
    *)
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

module Basic_lexxer = Compliant_lex.Make_lexxer(Json_parse_types.Basic)
module Basic_parser = Parser.Make(Json_parse_types.Basic)

let lexit filename contents =
  let lexbuf = Lexing.from_string contents in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  lex_and_print lexbuf

let parsit filename contents =
  let lexbuf = Lexing.from_string contents in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  match Basic_parser.lax Basic_lexxer.read lexbuf with
  | Error s -> begin
    let loc = Lexxer.error_pos_msg lexbuf in
    match Basic_lexxer.lex_error () with
    | None -> printf "%s at %s\n" s loc
    | Some e -> printf "%s: %s at %s\n" s e loc
    end
  | Ok json ->
    match json with
    | None -> printf "(*None*)\n";
    | Some json ->  print_json_value json; printf "\n"

let testit filename contents =
  let lexbuf = Lexing.from_string contents in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  match Basic_parser.lax Basic_lexxer.read lexbuf with
  | Error s -> begin
    match Basic_lexxer.lex_error () with
    | None -> printf "%s\n" s
    | Some e -> printf "%s: %s\n" s e
    end
  | Ok _ -> ()


module IO = struct
  type 'a t = 'a
  
  let return v = v
  let (>>=) a f = f a
end

module New_basic_lexxer = Compliant_lex.Make_lexxer(Json_parse_types.Basic)
module New_basic_parser_monad = Parser_monad.Make(Json_parse_types.Basic) (IO)
module New_basic_parser2 = Parser_basic.Make(Json_parse_types.Basic)
module New_basic_parser2_nola = Parser_basic_nola.Make(Json_parse_types.Basic)

let parsit2 filename contents =
  let lexbuf = Lexing.from_string contents in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let open IO in
  let reader () = return (New_basic_lexxer.read lexbuf) in
  New_basic_parser2.lax ~reader
  >>= function
    | Ok None -> Printf.printf "(*None*)\n"
    | Ok (Some json) -> print_json_value json; printf "\n"
    | Error s ->
      let loc = Lexxer.error_pos_msg lexbuf in
      printf "%s at %s\n" s loc

let testit2 filename contents =
  let lexbuf = Lexing.from_string contents in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let reader () = New_basic_lexxer.read lexbuf in
  match New_basic_parser2_nola.lax ~reader with
  | Ok None -> ()
  | Ok (Some json) -> ()
  | Error s ->
    let loc = Lexxer.error_pos_msg lexbuf in
      printf "%s at %s\n" s loc

(*
let () = 
  if Array.length Sys.argv < 2 then
    printf "expected filename\n"
  else
    let filename = Sys.argv.(1) in
    let contents = load_file filename in
      parsit2 filename contents
    (*
      parsit "../test.json"
      lexit "../test.json"
    *)
*)

(* module Json_basic = Jsonxt_monad.Make(Json_parse_types.Basic) *)

open Core
open Core_bench.Std

module Yj = struct
  open Yojson

  let read contents = Raw.from_string contents

  let benchit filename = 
    let contents = load_file filename in
    (fun () -> ignore (read contents))
end
  
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
  let reader () = New_basic_lexxer.read lexbuf in
    lex_and_discard reader


let benchit filename = 
  let contents = load_file filename in
  (fun () -> ignore(testit2 filename contents))

let benchit_lex filename = 
  let contents = load_file filename in
  (fun () -> ignore(testit_lex filename contents))

let testxt = benchit "../test.json.10000"
let testxt_lex = benchit_lex "../test.json.10000"
let testyj = Yj.benchit "../test.json.10000"

let () = Command.run (Bench.make_command [
    Bench.Test.create ~name:"lexxer" testxt_lex
  ; Bench.Test.create ~name:"parser" testxt
  ; Bench.Test.create ~name:"yojson" testyj
  ])

(*
*)
