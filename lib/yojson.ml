module Basic = struct
  module Compliance = struct
    type json = Json.Basic.json

    open Tokens

    let lex_number token = token
    let lex_integer token = token
    let lex_largeint _ = COMPLIANCE_ERROR "Integer out of bounds"

    let lex_variant _ = false
    let lex_tuple _ = false

    let number_to_string f =
      match classify_float f with
      | FP_normal | FP_subnormal | FP_zero ->
        Json_float.string_of_float_fast_int f
      | FP_infinite ->
        if f < 0. then "-Infinity" else "Infinity"
      | FP_nan ->
        "NaN"

    let largeint s = `Float (float_of_string s)
    let integer i = `Int i
    let null = `Null
    let string s = `String s
    let bool b = `Bool b
    let assoc a = `Assoc a
    let list l = `List l
    let tuple l = raise (Failure "tuples not supported in yojson basic mode")
    let variant l = raise (Failure "variants not supported in yojson basic mode")

    let number = function
    | `Float f ->     `Float f
    | `Infinity ->    `Float (1.0 /. 0.0)
    | `Neginfinity -> `Float (-1.0 /. 0.0)
    | `Nan ->         `Float (0.0 /. 0.0)

  end

  module Lexxer = Compliant_lexxer.Make(Compliance)
  module Parser = Parser.Make(Compliance)
  include Reader_string_file.Make (Lexxer) (Parser)
  type t = json

  include Writer_string.Make(Compliance)
  include Writer_file.Make(Compliance)
end

module Safe = struct
  module Compliance = struct
    type json = Json.Extended.json

    open Tokens

    let lex_number token = token
    let lex_integer token = token
    let lex_largeint token = token

    let lex_variant _ = true
    let lex_tuple _ = true

    let number_to_string f =
      match classify_float f with
      | FP_normal | FP_subnormal | FP_zero ->
        Json_float.string_of_float_fast_int f
      | FP_infinite ->
        if f < 0. then "-Infinity" else "Infinity"
      | FP_nan ->
        "NaN"

    let largeint s = `Intlit s
    let integer i = `Int i
    let null = `Null
    let string s = `String s
    let bool b = `Bool b
    let assoc a = `Assoc a
    let list l = `List l
    let tuple l = `Tuple l
    let variant k v = `Variant (k, v)

    let number = function
    | `Float f ->     `Float f
    | `Infinity ->    `Float (1.0 /. 0.0)
    | `Neginfinity -> `Float (-1.0 /. 0.0)
    | `Nan ->         `Float (0.0 /. 0.0)

  end

  module Lexxer = Compliant_lexxer.Make(Compliance)
  module Parser = Parser.Make(Compliance)
  include Reader_string_file.Make (Lexxer) (Parser)
  type t = json

  include Writer_string.Make(Compliance)
  include Writer_file.Make(Compliance)
end
