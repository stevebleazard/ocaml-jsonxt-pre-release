module type Intf = sig
  val pp : Format.formatter -> 'a Json_internal.constrained -> unit
  val to_string : 'a Json_internal.constrained -> string
  val to_channel : out_channel -> 'a Json_internal.constrained -> unit
end

module Make (Compliance : Compliance.S) : Intf
