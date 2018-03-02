module type S = sig
  val lex_number : Tokens.token -> Tokens.token
  val lex_integer : Tokens.token -> Tokens.token
  val lex_largeint : Tokens.token -> Tokens.token
end

module type Lex = sig
  val read : Lexing.lexbuf -> Tokens.token
  val lex_error : unit -> string option
  val set_lexbuf : Lexing.lexbuf -> unit
  val get_lexbuf : unit -> Lexing.lexbuf option
end

module Make_lexxer ( Compliant_lex : S  ) : Lex
