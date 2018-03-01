{
  (* CR sbleazard: It's not clear this has a major impact with the rule set used here

  module Lexing = struct
    (* Override the Lexing.engine to avoid creating a new position record
       each time a rule is matched. Reduces total parse time by around 30%.
       Idea stollen from yojson *)

    include Lexing

    external c_engine : lex_tables -> int -> lexbuf -> int = "caml_lex_engine"

    let engine tbl state buf =
      let result = c_engine tbl state buf in
      (*
      if result >= 0 then begin
        buf.lex_start_p <- buf.lex_curr_p;
        buf.lex_curr_p <- {buf.lex_curr_p
                           with pos_cnum = buf.lex_abs_pos + buf.lex_curr_pos};
      end;
      *)
      result
  end
  *)
  open Lexing
  open Tokens

  let string2num s =
    try (INT (int_of_string s)) with
    | Failure _ -> FLOAT (float_of_string s)

  exception SyntaxError of string

}

let digit_1_to_9 = ['1'-'9']
let digit = ['0'-'9']
let digits = digit+
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let posint = (digit | digit_1_to_9 digits)
let integer = '-'? posint
let frac = '.' digits
let e = ['e' 'E']['+' '-']?
let exp = e digits
let fp = '-'? (posint frac | posint exp | posint frac exp)
let unescaped_char = [ ' '-'!' '#'-'[' ']'-'~' '\128'-'\255' ]
let escaped_char = '\\' [ '"' '\\' 'b' 'f' 'n' 'r' 't' ]
let unicode_char = "\\u" hex_digit hex_digit hex_digit hex_digit
let character = (unescaped_char | escaped_char | unicode_char)
let characters = character+
let ident = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let double_quote = ['"']
let whitespace = [ ' ' '\t' '\r' ]+
let newline = '\n'
let nan = ( "nan" | "-nan" | "NaN" )
let inifinity = ( "inf" | "Infinity" )

rule read =
  parse
  | "true"
    { BOOL true }
  | "false"
    { BOOL false }
  | "null"
    { NULL }
  | "{"
    { OS }
  | "}"
    { OE }
  | "["
    { AS }
  | "]"
    { AE }
  | ","
    { COMMA }
  | ":"
    { COLON }
  | "-" inifinity
    { NEGINFINITY }
  | inifinity
    { INFINITY }
  | nan
    { NAN }
  | integer
    { string2num (Lexing.lexeme lexbuf) }
  | fp
    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | double_quote double_quote
    { STRING "" }
  | double_quote characters double_quote
    { STRING (Lexing.lexeme lexbuf) }
  | eof
    { EOF }
  | whitespace
    { read lexbuf }
  | newline
    { Lexing.new_line lexbuf; read lexbuf; }
  | _
    { raise (SyntaxError ("Unexpected char: " ^ (Lexing.lexeme lexbuf))) }

{

  let lex_and_print lexbuf =
    let rec loop () =
      match read lexbuf with
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
    match Basic_parser.lax read lexbuf with
    | None -> printf "Parse failed\n"
    | Some json ->  print_json_value json; printf "\n"

  let () = parsit "test.json"
  (*
  let () = lexit "test.json"
  open Core
  open Core_bench.Std

  let () = Command.run (Bench.make_command [Bench.Test.create ~name:"lexxer" (fun () -> Lexxer.lexit "test.json")])
  *)
}
