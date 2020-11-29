external format_float : string -> float -> string = "caml_format_float"
external format_int : string -> int -> string = "caml_format_int"

let string_of_float_json f =
  let int_value = int_of_float f in
  if abs int_value <= 9007199254740991 then (* IEEE max int in a float *)
    format_int "%lld" int_value
  else
    format_float "%.17g" f
