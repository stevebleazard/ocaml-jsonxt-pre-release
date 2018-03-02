module type S = sig
  val lex_number : Tokens.token -> Tokens.token
  val lex_integer : Tokens.token -> Tokens.token
  val lex_largeint : Tokens.token -> Tokens.token
end

module type Lex = sig
  val read : Lexing.lexbuf -> Tokens.token
  val lex_error : unit -> string option
end

module Make_lexxer ( Compliant_lex : S  ) : Lex = struct
  let error = ref None
  let lex_error () = !error

  let read lexbuf =
    let token = match Lexxer.read lexbuf with
      | INFINITY | NEGINFINITY | NAN | FLOAT _ as token -> Compliant_lex.lex_number token
      | INT _ as token -> Compliant_lex.lex_integer token
      | LARGEINT _ as token -> Compliant_lex.lex_largeint token
      | _ as token -> token
    in
    match token with
    | COMPLIANCE_ERROR err | LEX_ERROR err as token ->
      error := Some err;
      token
    | _ as token -> token
end
