type json_stream = Json_stream.Strict.json

include (Reader_stream.Reader_stream with type json_stream := json_stream)
include (Writer_stream_intf.Intf with type json_stream := json_stream)
