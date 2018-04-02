type json = Json.Extended.json
type t = json

include (Json_string_file.Json_string_file with type json := json)
include (Json_writer_intf.Intf with type json := Json.Extended.json)
