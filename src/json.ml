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

  let integer i = Some (`Int i)
  let null = `Null
  let string s = `String s
  let bool b = `Bool b
  let assoc a = `Assoc a
  let list l = `List l

  let number = function
  | `Float f ->     Some (`Float f)
  | `Infinity ->    Some (`Float (1.0 /. 0.0))
  | `Neginfinity -> Some (`Float (-1.0 /. 0.0))
  | `Nan ->         Some (`Float (0.0 /. 0.0))

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

  let integer i = Some (`Int i)
  let null = `Null
  let string s = `String s
  let bool b = `Bool b
  let assoc a = `Assoc a
  let list l = `List l

  let number = function
  | `Float f ->     Some (`Float f)
  | `Infinity ->    None
  | `Neginfinity -> None
  | `Nan ->         None

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

  let integer i = Some (`Float (float_of_int i))
  let null = `Null
  let string s = `String s
  let bool b = `Bool b
  let assoc a = `Assoc a
  let list l = `List l

  let number = function
  | `Float f ->     Some (`Float f)
  | `Infinity ->    None
  | `Neginfinity -> None
  | `Nan ->         None
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
