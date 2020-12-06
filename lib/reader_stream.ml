module type Reader_stream = sig
  type json_stream
  type stream

  val json_stream_of_string : string -> stream
  val json_stream_of_channel : in_channel -> stream
  val decode_stream : stream -> (json_stream option, string) result
end

module Make (Lexxer : Compliant_lexxer.Lex ) (Parser : Parser_stream.Parser) : Reader_stream
  with type json_stream = Parser.Compliance.json_stream
= struct
  type json_stream = Parser.Compliance.json_stream
  type stream = Parser.t

  let create_parser ~lexbuf =
    let reader () = Lexxer.read lexbuf in
      Parser.create ~reader

  let json_stream_of_string s =
    let lexbuf = Lexing.from_string s in
      create_parser ~lexbuf

  let json_stream_of_channel inc =
    let lexbuf = Lexing.from_channel inc in
      create_parser ~lexbuf

  let decode_stream t = Parser.decode t

end
