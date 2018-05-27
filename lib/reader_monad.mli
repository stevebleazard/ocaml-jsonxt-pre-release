module type Reader_monad = sig
  module IO : Io.IO
  type json

  val read_json : reader:(Bytes.t -> int -> int IO.t) -> (json, string) result IO.t
end

module Make  (Parser : Parser_monad.Parser) : Reader_monad
  with type json = Parser.Compliance.json
  and module IO := Parser.IO

