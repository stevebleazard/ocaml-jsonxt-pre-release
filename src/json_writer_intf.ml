module type Intf = sig
  type json

  val json_to_string : json -> (string, string) result
  val json_to_string_exn : json -> string
  val to_string : json -> string
  val json_to_file : string -> json -> (unit, string) result
  val json_to_file_exn : string -> json -> unit
  val json_to_channel :  out_channel -> json -> (unit, string) result
  val json_to_channel_exn :  out_channel -> json -> unit
end

