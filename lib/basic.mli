(** [Basic] supports parsing and writing JSON data that conforms to the
    [Basic] json type.  *)

type json = Json.Basic.json
type t = json

(** {1 Reader functions} *)
include (Reader_string_file.Reader_string_file with type json := json)

(** {1 Writer functions} *)
include (Writer_intf.Intf with type json := Json.Basic.json)

module Compliance : Compliance.S with
  type json = Json.Basic.json
  and type json_stream = Json_stream.Basic.json
