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
    | `As
    | `Ae
    | `Os
    | `Oe
    | `Ts
    | `Te
    | `Vs
    | `Ve
    | `Name of string
    | `Infinity
    | `Neg_infinity
    | `Nan
    ]
type t = json

module Extended : sig
  type nonrec json = json
  type t = json
end

module Basic : sig
  type json =
      [
      | `Null
      | `Bool of bool
      | `Int of int
      | `Float of float
      | `String of string
      | `As
      | `Ae
      | `Os
      | `Oe
      | `Name of string
      ]
  type t = json
end

module Strict : sig
  type json =
      [
      | `Null
      | `Bool of bool
      | `Float of float
      | `String of string
      | `As
      | `Ae
      | `Os
      | `Oe
      | `Name of string
      ]
  type t = json
end
