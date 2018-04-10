module Basic : sig
  type json = Json.Basic.json
  type t = json

  include (Reader_string_file.Reader_string_file with type json := json)
  include (Writer_intf.Intf with type json := Json.Basic.json)
end

module Safe : sig
  type json = Json.Extended.json
  type t = json

  include (Reader_string_file.Reader_string_file with type json := json)
  include (Writer_intf.Intf with type json := Json.Extended.json)
end
