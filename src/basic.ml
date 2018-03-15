module Lexxer = Compliant_lexxer.Make(Json_parse_types.Basic)
module Parser = Parser.Make(Json_parse_types.Basic)

include Json_string_file.Make (Lexxer) (Parser)

let json_to_string = Json_writer.to_string
let to_string = Json_writer.to_string
