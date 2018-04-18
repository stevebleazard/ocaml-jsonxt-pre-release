module Lexxer = Compliant_lexxer.Make(Compliance.Strict)
module Parser = Parser.Make(Compliance.Strict)
include Reader_string_file.Make (Lexxer) (Parser)
type t = json

include Writer_string.Make(Compliance.Strict)
include Writer_file.Make(Compliance.Strict)
