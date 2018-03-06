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

  let error_pos_msg (lexbuf : Lexing.lexbuf) =
    let start = lexbuf.lex_start_p in
    let cnum = lexbuf.lex_last_pos - start.pos_bol in
    let enum = lexbuf.lex_curr_pos - start.pos_bol in
      Printf.sprintf "line %d chars %d-%d" start.pos_lnum cnum enum

  let string2num s =
    try (INT (int_of_string s)) with
    | Failure _ -> LARGEINT s
  
  let update_pos lexbuf =
    let pos = lexbuf.lex_start_p in
    lexbuf.lex_start_p <-
      { pos with pos_bol = lexbuf.lex_start_pos;
                 pos_lnum = pos.pos_lnum + 1
      }

  let int_of_hexchar c =
    match c with
    | '0'..'9' -> int_of_char c - int_of_char '0'
    | 'a'..'f' -> int_of_char c - int_of_char 'a' + 10
    | 'A'..'F' -> int_of_char c - int_of_char 'A' + 10
    | _ -> assert false

  exception Lex_error of string

  let lex_error err = raise (Lex_error err)
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
  | double_quote 
    { read_string (Buffer.create 100) lexbuf }
  | eof
    { EOF }
  | whitespace
    { read lexbuf }
  | newline
    { update_pos lexbuf; read lexbuf; }
  | _
    { lex_error ("unexpected character '" ^ (Lexing.lexeme lexbuf) ^ "'") }

and read_string buf =
  parse
  | double_quote { STRING (Buffer.contents buf) }
  | '\\' '"'     { Buffer.add_char buf '"'; read_string buf lexbuf }
  | '\\' '\\'    { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'     { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'     { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'     { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'     { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'     { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | newline      { update_pos lexbuf; Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'u' (hex_digit as a) (hex_digit as b) (hex_digit as c) (hex_digit as d)
    { 
      let u =
        ((int_of_hexchar a) lsl 12) lor ((int_of_hexchar b) lsl 8) lor
        ((int_of_hexchar c) lsl 4) lor ((int_of_hexchar d) lsl 0)
      in
      if u >= 0xD800 && u <= 0xDBFF then
        second_of_surrogate_pair buf u lexbuf
      else
        Utf8.utf8_of_code buf u;

      read_string buf lexbuf
    }
  | [^ '"' '\\' '\n']+
    {
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | '\\' _       { lex_error ("unexepted escape sequence " ^ (Lexing.lexeme lexbuf)) }
  | eof          { lex_error "end of file" }

and second_of_surrogate_pair buf high =
  parse
  | '\\' 'u' (hex_digit as a) (hex_digit as b) (hex_digit as c) (hex_digit as d)
    {
      let low =
        ((int_of_hexchar a) lsl 12) lor ((int_of_hexchar b) lsl 8) lor
        ((int_of_hexchar c) lsl 4) lor ((int_of_hexchar d) lsl 0)
      in
      if low >= 0xDC00 && low <= 0xDFFF then
        Utf8.utf8_of_surrogate_pair buf high low 
      else
        lex_error ("invalid low surrogate for code point beyond U+FFFF " ^ (Lexing.lexeme lexbuf))
    }
  | '\\' _ 
    {
      lex_error ("expecting \\uXXXX escape sequence containing low surrogate for code point beyond U+FFFF, got '"
        ^ (Lexing.lexeme lexbuf) ^ "'")
    }
  | eof
    { lex_error "end of file" }

