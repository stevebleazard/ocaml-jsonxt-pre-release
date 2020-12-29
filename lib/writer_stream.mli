module type Intf = sig
  type t

  val create_encoder'
    :  add_char:(char -> unit)
    -> add_string:(string -> unit)
    -> incr:int
    -> eol:string
    -> t

  val create_encoder
    :  add_char:(char -> unit)
    -> add_string:(string -> unit)
    -> t

  val create_encoder_hum
    :  add_char:(char -> unit)
    -> add_string:(string -> unit)
    -> t

  val create_encoder_channel : out_channel -> t
  val create_encoder_channel_hum : out_channel -> t

  val encode_stream_exn : t -> 'a Json_internal.constrained_stream -> unit
  val encode_stream : t -> 'a Json_internal.constrained_stream -> (unit, string) result
end

module Make (Compliance : Compliance.S) : Intf
