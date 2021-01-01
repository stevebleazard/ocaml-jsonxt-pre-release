module type Intf = sig
  module IO : Io.IO
  type json

  val json_writer
       : writer:(string -> unit IO.t)
      -> eol:string
      -> incr:int
      -> json
      -> unit IO.t
  val write_json : writer:(string -> unit IO.t) -> json -> unit IO.t
  val write_json_hum : writer:(string -> unit IO.t) -> json -> unit IO.t
end
