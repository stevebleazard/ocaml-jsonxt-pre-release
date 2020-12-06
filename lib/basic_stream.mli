type json_stream = Json_stream.Basic.json

include (Reader_stream.Reader_stream with type json_stream := json_stream)
