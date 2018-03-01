let lex_and_print lexbuf =
  let rec loop () =
    match Lexxer.read lexbuf with
    | FLOAT f ->  Printf.printf "Float [%g]\n" f; loop ()
    | INT i ->  Printf.printf "Int [%d]\n" i; loop ()
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

module Basic_parser = Parser.Make(Json.Basic)
module Basic_lexxer = Compliant_lex.Make_lexxer(Json.Basic)

let lexit filename =
  let inf = open_in filename in
  let lexbuf = Lexing.from_channel inf in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  lex_and_print lexbuf;
  close_in inf

let parsit filename =
  let inf = open_in filename in
  let lexbuf = Lexing.from_channel inf in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  match Basic_parser.lax Basic_lexxer.read lexbuf with
  | Error s -> printf "%s\n" s
  | Ok json ->
    match json with
    | None -> printf "(*None*)\n";
    | Some json ->  print_json_value json; printf "\n"

let () = parsit "test.json"
(*
let () = lexit "test.json"
open Core
open Core_bench.Std

let () = Command.run (Bench.make_command [Bench.Test.create ~name:"lexxer" (fun () -> Lexxer.lexit "test.json")])
*)
