module Lexxer = Compliant_lexxer.Make(Json_parse_types.Yojson)
module Parser = Parser.Make(Json_parse_types.Yojson)

include Json_string_file.Make (Lexxer) (Parser)
