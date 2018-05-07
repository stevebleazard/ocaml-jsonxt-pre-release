module type IO = Io.IO

module type LexIO = sig
  module IO : IO

  val on_refill : Lexing.lexbuf -> unit IO.t
end

module type Lex = sig
  module IO : IO

  val read : Lexing.lexbuf -> (Tokens.token, string) result IO.t
end

module Make (Compliance : Compliance.S) (LexIO : LexIO) : Lex with module IO := LexIO.IO
