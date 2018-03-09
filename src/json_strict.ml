module Lexxer = Compliant_lexxer.Make(Json_parse_types.Strict)
module Parser = Parser.Make(Json_parse_types.Strict)

include Json_string_file.Make (Lexxer) (Parser)
