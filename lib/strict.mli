type json = Json.Strict.json
type t = json

include (Reader_string_file.Reader_string_file with type json := json)
include (Writer_intf.Intf with type json := Json.Strict.json)
