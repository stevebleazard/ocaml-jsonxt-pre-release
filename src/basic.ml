module Compliance = struct
  type json = Json.Basic.json
  type t = json

  open Tokens

  let lex_number = function
  | INFINITY -> COMPLIANCE_ERROR "inf not supported"
  | NEGINFINITY -> COMPLIANCE_ERROR "-inf not supported"
  | NAN -> COMPLIANCE_ERROR "nan not supported"
  | FLOAT _ as token -> token
  | _ as token -> token

  let lex_integer token = token (* CR sbleazard: fix bounds *)

  let lex_largeint = function
  | LARGEINT s -> FLOAT (float_of_string s)
  | token -> token

  let number_to_string f =
    match classify_float f with
    | FP_normal | FP_subnormal | FP_zero -> Floats.string_of_float_fast_int f
    | FP_infinite -> raise (Failure "infinity not supported")
    | FP_nan -> raise (Failure "nan not supported")

  let integer i = `Int i
  let null = `Null
  let string s = `String s
  let bool b = `Bool b
  let assoc a = `Assoc a
  let list l = `List l

  let number = function
  | `Float f ->     `Float f
  | `Infinity ->    `Null
  | `Neginfinity -> `Null
  | `Nan ->         `Null

end

module Lexxer = Compliant_lexxer.Make(Compliance)
module Parser = Parser.Make(Compliance)
include Json_string_file.Make (Lexxer) (Parser)

include Json_writer_string.Make(Compliance)
let to_string = json_to_string
