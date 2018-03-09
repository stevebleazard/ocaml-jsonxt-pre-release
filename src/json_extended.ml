module Lexxer = Compliant_lexxer.Make(Json_parse_types.Extended)
module Parser = Parser.Make(Json_parse_types.Extended)

include Json_string_file.Make (Lexxer) (Parser)
