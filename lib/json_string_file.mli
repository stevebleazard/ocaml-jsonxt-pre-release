module type Json_string_file = sig
  type json

  (** [json_of_string string] converts [string] to a [json] value returing an error
      if the string has syntax, grammar or compliance errors *)
  val json_of_string : string -> (json, string) result

  (** [json_of_string_exn string] converts [string] to a [json] value raising a
      [Failure] exception if the string has syntax, grammar or compliance errors *)
  val json_of_string_exn : string -> json

  (** [json_of_file file] converts the text from [file] to a [json] value returing an error
      if the file contents have syntax, grammar or compliance errors. The file is closed on error *)
  val json_of_file : string -> (json, string) result

  (** [json_of_file file] converts the text from [file] to a [json] value raising
      a [Failure] exception if the file contents have syntax, grammar or compliance errors.
      The file is closed on error *)
  val json_of_file_exn : string -> json

  (** [json_of_channel channel] converts the text from [channel] to a [json] value
      returing an error if the channel contents have syntax, grammar or compliance errors.
      The channel is not closed *)
  val json_of_channel : in_channel -> (json, string) result

  (** [json_of_channel channel] converts the text from [channel] to a [json] value raising
      a [Failure] exception if the channel contents have syntax, grammar or compliance errors.
      The file is not closed *)
  val json_of_channel_exn : in_channel -> json

  (** [of_string] is an alias for json_of_string_exn *)
  val of_string : string -> json

  (** [of_file] is an alias for json_of_file_exn *)
  val of_file : string -> json

  (** [of_channel] is an alias for json_of_channel_exn *)
  val of_channel : in_channel -> json
end

module Make (Lexxer : Compliant_lexxer.Lex ) (Parser : Parser.Parser) : Json_string_file
  with type json = Parser.Compliance.json

