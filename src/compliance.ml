module type S = sig
  type json

  val lex_number : Tokens.token -> Tokens.token
  val lex_integer : Tokens.token -> Tokens.token
  val lex_largeint : Tokens.token -> Tokens.token

  val number_to_string : float -> string

  val number : [`Float of float | `Infinity | `Neginfinity | `Nan ] -> json
  val integer : int -> json
  val null : json
  val string : string -> json
  val bool : bool -> json
  val assoc : (string * json) list -> json
  val list : json list -> json
end
