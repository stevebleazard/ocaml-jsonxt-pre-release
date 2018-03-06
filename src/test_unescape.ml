
  exception Lex_error of string

  let lex_error err = raise (Lex_error err)

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
         | _    -> lex_error ("invalid escape in string: '" ^ s ^ "'")
        end;
      | `U1_h1 -> u1 := int_of_hexchar s.[i];  state := `U1_h2
      | `U1_h2 -> u1 := (!u1 lsl 4) lor (int_of_hexchar s.[i]);  state := `U1_h3
      | `U1_h3 -> u1 := (!u1 lsl 4) lor (int_of_hexchar s.[i]);  state := `U1_h4
      | `U1_h4 ->
        u1 := (!u1 lsl 4) lor (int_of_hexchar s.[i]);
        if !u1 >= 0xD800 && !u1 <= 0xDBFF then
          state := `U2_h1
        else begin
          j := utf8_of_code s' !j !u1;
          state := `Char
        end
      | `U2_h1 -> u2 := int_of_hexchar s.[i];  state := `U2_h2
      | `U2_h2 -> u2 := (!u1 lsl 4) lor (int_of_hexchar s.[i]);  state := `U2_h3
      | `U2_h3 -> u2 := (!u1 lsl 4) lor (int_of_hexchar s.[i]);  state := `U2_h4
      | `U2_h4 ->
        state := `Char;
        u2 := (!u2 lsl 4) lor (int_of_hexchar s.[i]);
        if !u2 >= 0xDC00 && !u2 <= 0xDFFF then
          j := utf8_of_surrogate_pair s' !j !u1 !u2 
        else
          lex_error ("invalid low surrogate for code point beyond U+FFFF '" ^ s ^ "'")
    done;
    begin
      match !state with
      | `Char -> ()
      | _ -> lex_error("end of string in escape sequence: '" ^ s ^ "'")
    end;
    if !j <> l then Bytes.unsafe_to_string (Bytes.sub s' 0 !j) else s

let run () =
  (* Printf.printf "[%s]\n%!" (unescape_string "escaped[\\\"\\\\\\b\\f\\n\\r\\t]") *)
  Printf.printf "[%s]\n%!" (unescape_string "xxx\\txxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")

let () = run ()
(*
*)

open Core
open Core_bench.Std


    (* ignore(unescape_string "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx") *)
    (* ignore(unescape_string "\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b\\b") *)
let benchit () = 
  for i = 0 to 99999 do
    ignore(unescape_string "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
  done

let () = Command.run (Bench.make_command [
  Bench.Test.create ~name:"parser" benchit
])
(*
*)
