external format_float : string -> float -> string = "caml_format_float"
external format_int : string -> int -> string = "caml_format_int"

(* Determine the size of an integer, handles 30bit, 62bit and Jsoo using 32bit ints *)
let max_json_int =
  let rec log2 n = if n <= 1 then 0 else 1 + log2(n asr 1) in
  let bits n = log2 n + 1 in
  if bits max_int > 53 then (1 lsl 53) - 1 else max_int

let max_json_int_as_float = float_of_int max_json_int

let string_of_float_json f =
  let is_int = (float_of_int (int_of_float f)) = f in
  if is_int && Float.abs f <= max_json_int_as_float then (* IEEE max int in a float when in 64bit int mode*)
    (let int_value = int_of_float f in format_int "%lld" int_value)
  else
    format_float "%.17g" f

let string_of_float_json_float f =
  format_float "%.17g" f

let string_of_float_printf f =
  Printf.sprintf "%.17g" f

open Core
open Core_bench

let value_int = (2.**53.-.1.)
let value_float = (2.**53.*.10.)

let () =
  Command.run (Command.group
    ~summary:"Float to string tests"
    [
      "int", Bench.make_command [
          Bench.Test.create ~name:"intopt" (fun () -> string_of_float_json value_int)
        ; Bench.Test.create ~name:"float" (fun () -> string_of_float_json_float value_int)
        ; Bench.Test.create ~name:"printf" (fun () -> string_of_float_printf value_int)
      ];
      "float", Bench.make_command [
          Bench.Test.create ~name:"intopt" (fun () -> string_of_float_json value_float)
        ; Bench.Test.create ~name:"float" (fun () -> string_of_float_json_float value_float)
        ; Bench.Test.create ~name:"printf" (fun () -> string_of_float_printf value_float)
      ]
    ])
