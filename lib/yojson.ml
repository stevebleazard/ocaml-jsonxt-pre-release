module Basic = struct
  module Lexxer = Compliant_lexxer.Make(Compliance.Yojson.Basic)
  module Parser = Parser.Make(Compliance.Yojson.Basic)
  include Reader_string_file.Make (Lexxer) (Parser)
  type t = json

  include Writer_string.Make(Compliance.Yojson.Basic)
  include Writer_file.Make(Compliance.Yojson.Basic)
end

module Safe = struct
  module Lexxer = Compliant_lexxer.Make(Compliance.Yojson.Safe)
  module Parser = Parser.Make(Compliance.Yojson.Safe)
  include Reader_string_file.Make (Lexxer) (Parser)
  type t = json

  include Writer_string.Make(Compliance.Yojson.Safe)
  include Writer_file.Make(Compliance.Yojson.Safe)
end
