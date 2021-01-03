(** [Strict_stream] supports parsing and writing JSON data that conforms to the
    [Json_stream.Strict.json] type as a stream of [json_stream] objects.  *)

type json_stream = Json_stream.Strict.json

(** {1 Reader functions} *)
include (Reader_stream.Reader_stream with type json_stream := json_stream)

(** {1 Writer functions} *)
include (Writer_stream_intf.Intf with type json_stream := json_stream)
