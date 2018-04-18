module Lexxer = Compliant_lexxer.Make(Compliance.Basic)
module Parser = Parser.Make(Compliance.Basic)
include Reader_string_file.Make (Lexxer) (Parser)
type t = json

include Writer_string.Make(Compliance.Basic)
include Writer_file.Make(Compliance.Basic)
