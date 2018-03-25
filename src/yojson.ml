module Compliance = struct
  type json = Json.json
  type t = json

  open Tokens

  let lex_number token = token
  let lex_integer token = token (* CR sbleazard: fix bounds *)
  let lex_largeint _ = COMPLIANCE_ERROR "Integer out of bounds"

  let integer i = `Int i
  let null = `Null
  let string s = `String s
  let bool b = `Bool b
  let assoc a = `Assoc a
  let list l = `List l

  let number = function
  | `Float f ->     `Float f
  | `Infinity ->    `Float (1.0 /. 0.0)
  | `Neginfinity -> `Float (-1.0 /. 0.0)
  | `Nan ->         `Float (0.0 /. 0.0)

end

module Lexxer = Compliant_lexxer.Make(Compliance)
module Parser = Parser.Make(Compliance)

include Json_string_file.Make (Lexxer) (Parser)
