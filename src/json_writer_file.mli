module type Intf = sig
  val json_to_file : string -> 'a Json_internal.constrained -> (unit, string) result
  val json_to_file_exn : string -> 'a Json_internal.constrained -> unit
  val json_to_channel :  out_channel ->  'a Json_internal.constrained -> (unit, string) result
  val json_to_channel_exn :  out_channel ->  'a Json_internal.constrained -> unit
end

module Make (Compliance : Compliance.S) : Intf