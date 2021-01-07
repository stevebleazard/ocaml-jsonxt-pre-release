external format_float : string -> float -> string = "caml_format_float"
external format_int : string -> int -> string = "caml_format_int"

(* Determine the size of an integer, handles 30bit, 62bit and Jsoo using 32bit ints *)
let max_json_int =
  let rec log2 n = if n <= 1 then 0 else 1 + log2(n asr 1) in
  let bits n = log2 n + 1 in
  if bits max_int > 53 then (1 lsl 53) - 1 else max_int

let string_of_float_json f =
  let int_value = int_of_float f in
  if abs int_value <= max_json_int then (* IEEE max int in a float when in 64bit int mode*)
    format_int "%lld" int_value
  else
    format_float "%.17g" f
