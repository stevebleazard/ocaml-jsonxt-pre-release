{
  module Lexing = struct
    (* Override the Lexing.engine to avoid creating a new position record
       each time a rule is matched. Reduces total parse time by around 30%.
       Idea stolen from yojson *)

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

  exception Lex_error of string

  let lex_error err = raise (Lex_error err)

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
    | c -> lex_error ("invalid hex char in unicode escape: '" ^ (String.make 1 c) ^ "'")

  
  let utf8_of_code buf idx u =
    let i = ref idx in

    let add_char c = Bytes.unsafe_set buf !i c; i := !i + 1 in

    let max_used_bits n u = (u lsr n) = 0 in

    if max_used_bits 7 u then
      add_char (Char.chr u)
    else if max_used_bits 11 u then begin
      add_char (Char.chr (0b11000000 lor ((u lsr 6) land 0b00011111)));
      add_char (Char.chr (0b10000000 lor (u         land 0b00111111)))
    end
    else if max_used_bits 16 u then begin
      add_char (Char.chr (0b11100000 lor ((u lsr 12) land 0b00001111)));
      add_char (Char.chr (0b10000000 lor ((u lsr  6) land 0b00111111)));
      add_char (Char.chr (0b10000000 lor (u          land 0b00111111)))
    end
    else if max_used_bits 21 u then begin
      add_char (Char.chr (0b11110000 lor ((u lsr 18) land 0b00000111)));
      add_char (Char.chr (0b10000000 lor ((u lsr 12) land 0b00111111)));
      add_char (Char.chr (0b10000000 lor ((u lsr  6) land 0b00111111)));
      add_char (Char.chr (0b10000000 lor (u          land 0b00111111)));
    end
    else if max_used_bits 26 u then begin
      add_char (Char.chr (0b11111000 lor ((u lsr 24) land 0b00000011)));
      add_char (Char.chr (0b10000000 lor ((u lsr 18) land 0b00111111)));
      add_char (Char.chr (0b10000000 lor ((u lsr 12) land 0b00111111)));
      add_char (Char.chr (0b10000000 lor ((u lsr  6) land 0b00111111)));
      add_char (Char.chr (0b10000000 lor (u          land 0b00111111)));
    end
    else begin
      add_char (Char.chr (0b11111100 lor ((u lsr 30) land 0b00000001)));
      add_char (Char.chr (0b10000000 lor ((u lsr 24) land 0b00111111)));
      add_char (Char.chr (0b10000000 lor ((u lsr 18) land 0b00111111)));
      add_char (Char.chr (0b10000000 lor ((u lsr 12) land 0b00111111)));
      add_char (Char.chr (0b10000000 lor ((u lsr  6) land 0b00111111)));
      add_char (Char.chr (0b10000000 lor (u          land 0b00111111)));
    end;
    !i

  let utf8_of_surrogate_pair buf idx high low =
    let high = high - 0xD800 in
    let low = low - 0xDC00 in
    let code = 0x10000 + ((high lsl 10) lor low) in
      utf8_of_code buf idx code

  let escaping_error msg s c off =
    let offs = string_of_int off in
    let cs =
      match c with
      | Some c -> " '" ^ (String.make 1 c) ^ "' "
      | None -> ""
    in
      lex_error (msg ^ " at offset " ^ offs ^ cs ^ ": " ^ s)

  let unescape_string s =
    let l = String.length s in
    let s' = Bytes.create l in
    let j = ref 0 in
    let u1 = ref 0 in
    let u2 = ref 0 in
    let state = ref `Char in

    for i = 0 to l - 1 do
      match !state with
      | `Char -> begin
           match s.[i] with
           | '\\' -> state := `Escape
           | c -> Bytes.unsafe_set s' !j c; j := !j + 1
         end;
      | `Escape -> begin
        match s.[i] with
         | '"'  -> Bytes.unsafe_set s' !j '"';    state := `Char; j := !j + 1
         | '\\' -> Bytes.unsafe_set s' !j '\\';   state := `Char; j := !j + 1
         | 'b'  -> Bytes.unsafe_set s' !j '\b';   state := `Char; j := !j + 1
         | 'f'  -> Bytes.unsafe_set s' !j '\012'; state := `Char; j := !j + 1
         | 'n'  -> Bytes.unsafe_set s' !j '\n';   state := `Char; j := !j + 1
         | 'r'  -> Bytes.unsafe_set s' !j '\r';   state := `Char; j := !j + 1
         | 't'  -> Bytes.unsafe_set s' !j '\t';   state := `Char; j := !j + 1
         | 'u'  -> state := `U1_h1
         | _    -> escaping_error "invalid escape in string" s (Some s.[i]) i
        end;
      | `U1_h1 -> u1 := int_of_hexchar s.[i];  state := `U1_h2
      | `U1_h2 -> u1 := (!u1 lsl 4) lor (int_of_hexchar s.[i]);  state := `U1_h3
      | `U1_h3 -> u1 := (!u1 lsl 4) lor (int_of_hexchar s.[i]);  state := `U1_h4
      | `U1_h4 ->
        u1 := (!u1 lsl 4) lor (int_of_hexchar s.[i]);
        if !u1 >= 0xD800 && !u1 <= 0xDBFF then
          state := `U2_bs
        else begin
          j := utf8_of_code s' !j !u1;
          state := `Char
        end
      | `U2_bs ->
        if s.[i] <> '\\' then
          escaping_error "expected low surrogate escape char (\\)" s (Some s.[i]) i
        else
          state := `U2_u
      | `U2_u ->
        if s.[i] <> 'u' then
          escaping_error "expected low surrogate escape sequence (u)" s (Some s.[i]) i
        else
          state := `U2_h1
      | `U2_h1 -> u2 := int_of_hexchar s.[i];  state := `U2_h2
      | `U2_h2 -> u2 := (!u2 lsl 4) lor (int_of_hexchar s.[i]);  state := `U2_h3
      | `U2_h3 -> u2 := (!u2 lsl 4) lor (int_of_hexchar s.[i]);  state := `U2_h4
      | `U2_h4 ->
        state := `Char;
        u2 := (!u2 lsl 4) lor (int_of_hexchar s.[i]);
        if !u2 >= 0xDC00 && !u2 <= 0xDFFF then
          j := utf8_of_surrogate_pair s' !j !u1 !u2 
        else
          escaping_error "invalid low surrogate for code point beyond U+FFFF'" s None i
    done;
    begin
      match !state with
      | `Char -> ()
      | _ -> escaping_error "end of string in escape sequence" s None l
    end;
    if !j <> l then Bytes.unsafe_to_string (Bytes.sub s' 0 !j) else s

module type Lex = sig
  val read : Lexing.lexbuf -> Tokens.token
end

module Make (Compliance : Compliance.S) : Lex = struct

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
    { Compliance.lex_number NEGINFINITY }
  | inifinity
    { Compliance.lex_number INFINITY }
  | "+" inifinity
    { Compliance.lex_number INFINITY }
  | nan
    { Compliance.lex_number NAN }
  | integer
    {
      match string2num (Lexing.lexeme lexbuf) with
      | INT _ as tok -> Compliance.lex_integer tok
      | LARGEINT _ as tok -> Compliance.lex_largeint tok
      | tok -> tok
    }
  | fp
    { Compliance.lex_number (FLOAT (float_of_string (Lexing.lexeme lexbuf))) }
  | double_quote double_quote
    { STRING "" }
  | double_quote characters double_quote
    { STRING (unescape_string (Lexing.lexeme lexbuf)) }
  | eof
    { EOF }
  | whitespace
    { read lexbuf }
  | newline
    { update_pos lexbuf; read lexbuf; }
  | _
    { lex_error ("unexpected character '" ^ (Lexing.lexeme lexbuf) ^ "'") }

{
end
}
