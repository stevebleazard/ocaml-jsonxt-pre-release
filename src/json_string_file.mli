module type Json_string_file = sig
  type json
  type t = json

  val json_of_string : string -> (json, string) result
  val json_of_string_exn : string -> json
  val json_of_file : string -> (json, string) result
  val json_of_file_exn : string -> json
  val of_string : string -> json
end

module Make (Lexxer : Compliant_lexxer.Lex ) (Parser : Parser.Parser) : Json_string_file
  with type json = Parser.Compliance.json

