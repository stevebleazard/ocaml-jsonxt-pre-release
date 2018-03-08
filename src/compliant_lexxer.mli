module type Lex = sig
  val read : Lexing.lexbuf -> Tokens.token
end

val error_pos_msg : Lexing.lexbuf -> string

module Make (Compliance : Compliance.S) : Lex
