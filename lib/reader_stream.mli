module type Reader_stream = sig
  type json_stream
  type stream

  val json_stream_of_string : string -> stream
  val json_stream_of_channel : in_channel -> stream
  val decode_stream : stream -> (json_stream option, string) result
  val stream_from_string : string -> json_stream Stream.t
  val stream_from_channel : in_channel -> json_stream Stream.t
end

module Make (Lexxer : Compliant_lexxer.Lex ) (Parser : Parser_stream.Parser) : Reader_stream
  with type json_stream = Parser.Compliance.json_stream
