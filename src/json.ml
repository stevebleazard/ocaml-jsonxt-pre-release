open Tokens

type json =
    [
    | `Null
    | `Bool of bool
    | `Int of int
    | `Intlit of string
    | `Float of float
    | `Floatlit of string
    | `String of string
    | `Stringlit of string
    | `Assoc of (string * json) list
    | `List of json list
    | `Tuple of json list
    | `Variant of (string * json option)
    ]
type t = json

module Extended = struct
  type nonrec json = json
  type t = json

  let lex_number token = token (* CR sbleazard: check finite *)
  let lex_integer token = token (* CR sbleazard: fix bounds? *)

  let lex_largeint = function
  | LARGEINT s ->  
    let f = float_of_string s in
    match classify_float f with
    | FP_normal | FP_subnormal | FP_zero -> FLOAT f
    | FP_infinite -> INFINITY
    | FP_nan -> NAN


  let integer i = Some (`Int i)
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

module Yojson = struct
  type nonrec json = json
  type t = json

  let lex_number token = token
  let lex_integer token = token (* CR sbleazard: fix bounds *)
  let lex_largeint _ = COMPLIANCE_ERROR "Integer out of bounds"

  let integer i = Some (`Int i)
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

module Basic = struct
  type json =
      [
      | `Null
      | `Bool of bool
      | `Int of int
      | `Float of float
      | `String of string
      | `Assoc of (string * json) list
      | `List of json list
      ]
  type t = json

  let lex_number = function
  | INFINITY -> COMPLIANCE_ERROR "inf not supported"
  | NEGINFINITY -> COMPLIANCE_ERROR "-inf not supported"
  | NAN -> COMPLIANCE_ERROR "nan not supported"
  | FLOAT _ as token -> token
  | _ as token -> token

  let lex_integer token = token (* CR sbleazard: fix bounds *)
  let lex_largeint _ = COMPLIANCE_ERROR "Integer out of bounds"

  let integer i = Some (`Int i)
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

module Strict = struct
  type json =
      [
      | `Null
      | `Bool of bool
      | `Float of float
      | `String of string
      | `Assoc of (string * json) list
      | `List of json list
      ]
  type t = json

  let lex_number = function
  | INFINITY -> COMPLIANCE_ERROR "inf not supported"
  | NEGINFINITY -> COMPLIANCE_ERROR "-inf not supported"
  | NAN -> COMPLIANCE_ERROR "nan not supported"
  | FLOAT _ as token -> token
  | _ as token -> token

  let lex_integer token = token (* CR sbleazard: fix bounds *)
  let lex_largeint _ = COMPLIANCE_ERROR "Integer out of bounds"

  let integer i = Some (`Float (float_of_int i))
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

module Stream = struct
  type json =
      [
      | `Null
      | `Bool of bool
      | `Int of int
      | `Intlit of string
      | `Float of float
      | `Floatlit of string
      | `String of string
      | `Stringlit of string
      | `Tuple of json list
      | `Variant of (string * json option)
      | `As
      | `Ae
      | `Os
      | `Oe
      | `Name of string
      | `Value of json
      | `Infinity
      | `Neg_infinity
      | `Nan
      ]
  type t = json
end
