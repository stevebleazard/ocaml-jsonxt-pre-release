module type Intf = sig
  type 'a json_all =
    [< `Assoc of (string * 'a) list
    | `Bool of bool
    | `Float of float
    | `Floatlit of string
    | `Int of int
    | `Intlit of string
    | `List of 'a list
    | `Null
    | `String of string
    | `Stringlit of string
    | `Tuple of 'a list
    | `Variant of string * 'a option ] as 'a

  val json_to_file : string -> 'a json_all -> unit
  val json_to_channel :  out_channel ->  'a json_all -> unit

end

module Make (Compliance : Compliance.S) : Intf
