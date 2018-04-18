module Lexxer = Compliant_lexxer.Make(Compliance.Extended)
module Parser = Parser.Make(Compliance.Extended)
include Reader_string_file.Make (Lexxer) (Parser)
type t = json

include Writer_string.Make(Compliance.Extended)
include Writer_file.Make(Compliance.Extended)
