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

  type token = [
  | `Null
  | `Bool of bool
  | `String of string
  | `Float of float
  | `Int of int
  | `Name of string
  | `As
  | `Ae
  | `Os
  | `Oe
  | `Comma
  | `Colon
  | `Eof
  | `Infinity
  | `Neg_infinity
  | `Nan
  ]


  let string2num s =
    try (`Int (int_of_string s)) with
    | Failure _ -> `Float (float_of_string s)

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
let unescaped_char = [ ' '-'!' '#'-'[' ']'-'~' ]
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
    { `Bool true }
  | "false"
    { `Bool false }
  | "null"
    { `Null }
  | "{"
    { `Os }
  | "}"
    { `Oe }
  | "["
    { `As }
  | "]"
    { `Ae }
  | ","
    { `Comma }
  | ":"
    { `Colon }
  | "-" inifinity
    { `Neg_infinity }
  | inifinity
    { `Infinity }
  | nan
    { `Nan }
  | integer
    { string2num (Lexing.lexeme lexbuf) }
  | fp
    { `Float (float_of_string (Lexing.lexeme lexbuf)) }
  | double_quote double_quote
    { `String "" }
  | double_quote characters double_quote
    { `String (Lexing.lexeme lexbuf) }
  | eof
    { `Eof }
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
      | `Float f ->  Printf.printf "Float [%g]\n" f; loop ()
      | `Int i ->  Printf.printf "Int [%d]\n" i; loop ()
      | `String s -> Printf.printf "String [%s]\n" s; loop ()
      | `Bool b ->  Printf.printf "Bool [%s]\n" (if b then "true" else "false"); loop ()
      | `Null ->  Printf.printf "Null\n"; loop ()
      | `As ->  Printf.printf "A_start\n"; loop ()
      | `Ae ->  Printf.printf "A_end\n"; loop ()
      | `Os ->  Printf.printf "O_start\n"; loop ()
      | `Oe ->  Printf.printf "O_end\n"; loop ()
      | `Colon ->  Printf.printf "Colon\n"; loop ()
      | `Comma ->  Printf.printf "Comma\n"; loop ()
      | `Infinity ->  Printf.printf "Infinity\n"; loop ()
      | `Neg_infinity ->  Printf.printf "Neg_infinity\n"; loop ()
      | `Nan ->  Printf.printf "Nan\n"; loop ()
      | `Eof -> Printf.printf "EOF\n"
      (*
      | _ ->  loop ()
      *)
    in
      loop ()

  let lexit filename =
    let inf = open_in filename in
    let lexbuf = Lexing.from_channel inf in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    lex_and_print lexbuf;
    close_in inf

  let () = lexit "test.json"
}
