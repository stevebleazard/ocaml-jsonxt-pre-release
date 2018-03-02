{
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
  open Lexing
  open Tokens

  let string2num s =
    try (INT (int_of_string s)) with
    | Failure _ -> LARGEINT s
  
  let update_pos lexbuf =
    let pos = lexbuf.lex_start_p in
    Printf.printf "pos:  bol=%d, lnum = %d, cnum = %d\n%!" pos.pos_bol pos.pos_lnum pos.pos_cnum;
    Printf.printf "cur:  cnum = %d\n%!" lexbuf.lex_curr_pos;
    lexbuf.lex_start_p <-
      { pos with pos_bol = lexbuf.lex_start_pos;
                 pos_lnum = pos.pos_lnum + 1
      }
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
  | "+" inifinity
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
    { update_pos lexbuf; read lexbuf; }
  | _
    { LEX_ERROR ("Unexpected char: " ^ (Lexing.lexeme lexbuf)) }
