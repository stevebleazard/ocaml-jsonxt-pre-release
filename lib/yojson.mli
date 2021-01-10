module Basic : sig
  type json = Json.Basic.json
  type t = json

  include (Reader_string_file.Reader_string_file with type json := json)
  include (Writer_intf.Intf with type json := Json.Basic.json)

  module Compliance : Compliance.S with
    type json = Json.Basic.json
    and type json_stream = Json_stream.Basic.json
end

module Safe : sig
  type json = Json.Extended.json
  type t = json

  include (Reader_string_file.Reader_string_file with type json := json)
  include (Writer_intf.Intf with type json := Json.Extended.json)

  module Compliance : Compliance.S with
    type json = Json.Extended.json
    and type json_stream = Json_stream.Extended.json
end

module Raw : sig
  type json = Json.Extended.json
  type t = json

  include (Reader_string_file.Reader_string_file with type json := json)
  include (Writer_intf.Intf with type json := Json.Extended.json)

  module Compliance : Compliance.S with
    type json = Json.Extended.json
    and type json_stream = Json_stream.Extended.json
end
