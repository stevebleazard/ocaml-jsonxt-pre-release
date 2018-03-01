module type S = sig
  val lex_number : Tokens.token -> Tokens.token
  val lex_integer : Tokens.token -> Tokens.token
end

module type Lex = sig
  val read : Lexing.lexbuf -> Tokens.token
  val lex_error : unit -> string option
end

module Make_lexxer ( Compliant_lex : S  ) : Lex
