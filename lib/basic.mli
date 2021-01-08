(** [Basic] supports parsing and writing JSON data that conforms to the
    {!type:Json.Basic.json} json type.  This includes support for
    integers which are not part of the JSON standard *)

type json = Json.Basic.json
type t = json

(** {1 Reader functions} *)
include (Reader_string_file.Reader_string_file with type json := json)

(** {1 Writer functions} *)
include (Writer_intf.Intf with type json := Json.Basic.json)

module Compliance : Compliance.S with
  type json = Json.Basic.json
  and type json_stream = Json_stream.Basic.json
