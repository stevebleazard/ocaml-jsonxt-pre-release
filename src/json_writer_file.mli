module type Intf = sig
  val json_to_file : string -> 'a Json_internal.constained -> unit
  val json_to_channel :  out_channel ->  'a Json_internal.constained -> unit
end

module Make (Compliance : Compliance.S) : Intf
