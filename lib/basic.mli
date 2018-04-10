type json = Json.Basic.json
type t = json

include (Reader_string_file.Reader_string_file with type json := json)
include (Json_writer_intf.Intf with type json := Json.Basic.json)
