module type Intf = sig
  val json_to_string : 'a Json_internal.constrained -> (string, string) result
  val json_to_string_exn : 'a Json_internal.constrained -> string
  val to_string : 'a Json_internal.constrained -> string
  val json_to_string_hum : 'a Json_internal.constrained -> (string, string) result
  val json_to_string_hum_exn : 'a Json_internal.constrained -> string
  val to_string_hum : 'a Json_internal.constrained -> string
end

module Make (Compliance : Compliance.S) : Intf
