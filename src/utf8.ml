
let utf8_of_code buf x =
  let add_char = Buffer.add_char buf in

  let max_used_bits n x = (x lsr n) = 0 in

  if max_used_bits 7 x then
    add_char (Char.chr x)
  else if max_used_bits 11 x then begin
    add_char (Char.chr (0b11000000 lor ((x lsr 6) land 0b00011111)));
    add_char (Char.chr (0b10000000 lor (x         land 0b00111111)))
  end
  else if max_used_bits 16 x then begin
    add_char (Char.chr (0b11100000 lor ((x lsr 12) land 0b00001111)));
    add_char (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
    add_char (Char.chr (0b10000000 lor (x          land 0b00111111)))
  end
  else if max_used_bits 21 x then begin
    add_char (Char.chr (0b11110000 lor ((x lsr 18) land 0b00000111)));
    add_char (Char.chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
    add_char (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
    add_char (Char.chr (0b10000000 lor (x          land 0b00111111)));
  end
  else if max_used_bits 26 x then begin
    add_char (Char.chr (0b11111000 lor ((x lsr 24) land 0b00000011)));
    add_char (Char.chr (0b10000000 lor ((x lsr 18) land 0b00111111)));
    add_char (Char.chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
    add_char (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
    add_char (Char.chr (0b10000000 lor (x          land 0b00111111)));
  end
  else begin
    add_char (Char.chr (0b11111100 lor ((x lsr 30) land 0b00000001)));
    add_char (Char.chr (0b10000000 lor ((x lsr 24) land 0b00111111)));
    add_char (Char.chr (0b10000000 lor ((x lsr 18) land 0b00111111)));
    add_char (Char.chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
    add_char (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
    add_char (Char.chr (0b10000000 lor (x          land 0b00111111)));
  end

let utf8_of_surrogate_pair buf high low =
  let high = high - 0xD800 in
  let low = low - 0xDC00 in
  let code = 0x10000 + ((high lsl 10) lor low) in
    utf8_of_code buf code

