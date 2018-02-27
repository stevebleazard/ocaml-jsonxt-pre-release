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

  type token = [
  | `Null
  | `Bool of bool
  | `String of string
  | `Float of float
  | `Int of int (* CR sbleazard: how to handle 32bit builds? *)
  | `Name of string
  | `As
  | `Ae
  | `Os
  | `Oe
  | `Comma
  | `Colon
  | `Eof ]


  exception SyntaxError of string

  let new_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
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
let unescaped_char = [ ' '-'!' '#'-'[' ']'-'~' ]
let escaped_char = '\\' [ '"' '\\' 'b' 'f' 'n' 'r' 't' ]
let unicode_char = "\\u" hex_digit hex_digit hex_digit hex_digit
let character = (unescaped_char | escaped_char | unicode_char)
let characters = character+
let ident = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let double_quote = ['"']
let whitespace = [ ' ' '\t' '\r' ]+
let newline = '\n'

rule read =
  parse
  | double_quote double_quote
    { `String "" }
  | double_quote characters double_quote
    { `String (Lexing.lexeme lexbuf) }
  | eof
    { `Eof }
  | whitespace
    { read lexbuf }
  | newline
    { new_line lexbuf; read lexbuf; }
  | _
    { raise (SyntaxError ("Unexpected char: " ^ (Lexing.lexeme lexbuf))) }

{

  let lex_and_print lexbuf =
    let rec loop () =
      match read lexbuf with
      | `String s -> Printf.printf "String [%s]\n" s; loop ()
      | `Eof -> Printf.printf "EOF\n"
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
