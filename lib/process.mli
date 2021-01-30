module Strict : sig
  include (Process_intf.Shared with type json := Json.Strict.json)
  include (Process_intf.Strict with type json := Json.Strict.json)
end

module Basic : sig
  include (Process_intf.Shared with type json := Json.Basic.json)
  include (Process_intf.Basic with type json := Json.Basic.json)
end

module Extended : sig
  include (Process_intf.Shared with type json := Json.Extended.json)
  include (Process_intf.Extended with type json := Json.Extended.json)
end

module Yojson_safe : sig
  type json =
    [
    | `Null
    | `Bool of bool
    | `Int of int
    | `Intlit of string
    | `Float of float
    | `String of string
    | `Assoc of (string * json) list
    | `List of json list
    | `Tuple of json list
    | `Variant of (string * json option)
    ]
  include (Process_intf.Shared with type json := json)
  include (Process_intf.Yojson_safe with type json := json)
end
