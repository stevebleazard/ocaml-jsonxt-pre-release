module type Json_string_file = sig
  type json

  (** [json_of_string string] convers [string] to a [json] value returing an error
      if the string has syntax, grammer or compliance error *)
  val json_of_string : string -> (json, string) result

  (** [json_of_string_exn string] convers [string] to a [json] value raising a
      Failure exception if the string has syntax, grammer or compliance error *)
  val json_of_string_exn : string -> json

  (** [json_of_file file] convers [file] to a [json] value returing an error
      if the file has syntax, grammer or compliance error. The file is closed on error *)
  val json_of_file : string -> (json, string) result

  (** [json_of_file file] convers [file] to a [json] value raising a Failure exception
      if the file has syntax, grammer or compliance error. The file is closed on error *)
  val json_of_file_exn : string -> json

  (** [of_string] is an alias for json_of_string_exn *)
  val of_string : string -> json
end

module Make (Lexxer : Compliant_lexxer.Lex ) (Parser : Parser.Parser) : Json_string_file
  with type json = Parser.Compliance.json

