module type Intf = sig
  val pp : ?std:bool -> Format.formatter -> 'a Json_internal.constrained -> unit
  val to_string : ?std:bool -> 'a Json_internal.constrained -> string
  val to_channel : ?std:bool -> out_channel -> 'a Json_internal.constrained -> unit
end

module Make (Compliance : Compliance.S) : Intf
