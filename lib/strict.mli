(** [Strict] supports parsing and writing JSON data that conforms to the
    {!type:Json.Strict.json} json type.  This only supports types
    supported by the JSON standard and explicity excludes integers *)

type json = Json.Strict.json
type t = json

(** {1 Reader functions} *)
include (Reader_string_file.Reader_string_file with type json := json)

(** {1 Writer functions} *)
include (Writer_intf.Intf with type json := Json.Strict.json)

module Compliance : Compliance.S with
  type json = Json.Strict.json
  and type json_stream = Json_stream.Strict.json
