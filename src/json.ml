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
    constraint json = Json_parse_types.json

type t = json

module Extended = struct
  type nonrec json = json
  type t = json
end

module Yojson = struct
  type nonrec json = json
  type t = json
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
      constraint json = Json_parse_types.Basic.json
  type t = json
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
      constraint json = Json_parse_types.Strict.json
  type t = json
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
      constraint json = Json_parse_types.Stream.json
  type t = json
end
