(** Json stream types for the various compliance levels *)

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
  (** [Extended] supports all Json types including the non-standard
      tuple and variant introduced by [Yojson] *)

  type nonrec json = json
  type t = json
end

module Basic : sig
  (** [Basic] supports standard Json types that are supported by the
      JSON standard but also supports integers rather than just floats *)

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
  (** [Strict] supports only types that are supported by the JSON standard.
      Integers are not supported *)

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
