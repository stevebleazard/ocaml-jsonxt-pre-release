module type Parser = sig
  module Compliance : Compliance.S

  val lax
    :  reader : (unit -> Tokens.token)
    -> (Compliance.json option, string) result

  val ecma404
    :  reader : (unit -> Tokens.token)
    -> (Compliance.json option, string) result

end

module Make (Compliance : Compliance.S) : Parser
  with module Compliance := Compliance
= struct

  exception Parse_error of [`Eof | `Syntax_error of string]

  (* CR sbleazard: fix *)
  let token_error tok =
    let open Tokens in
    let err = match tok with
      | STRING s -> "unexpected string '" ^ s ^ "'"
      | OS -> "unexpected '{'"
      | OE -> "unexpected '}'"
      | NULL -> "unexpected null value"
      | NEGINFINITY -> "unexpected negative infinity"
      | NAN -> "unexpected Not-a-Number"
      | LEX_ERROR s -> s
      | LARGEINT s -> "unexpected large integer '" ^ s ^ "'"
      | INT i -> "unexpected integer '" ^ (string_of_int i) ^ "'"
      | INFINITY -> "unexpected infinity"
      | FLOAT f -> "unexpected float '" ^ (string_of_float f) ^ "'"
      | EOF -> "unexpected end-of-file"
      | COMPLIANCE_ERROR s -> "compliance error '" ^ s ^ "'"
      | COMMA -> "unexpected ','"
      | COLON -> "unexpected ':'"
      | BOOL b -> "unexpected boolean '" ^ (if b then "true" else "false") ^ "'"
      | AS -> "unexpected '['"
      | AE -> "unexpected ']'"
    in
      `Syntax_error err
   
   let parse_error tok = raise (Parse_error (token_error tok))

  let json_value ~reader = 
    let open Tokens in
    let rec token_value tok = begin
      match tok with
      | INT i -> Compliance.integer i
      | STRING s -> Compliance.string s
      | BOOL b -> Compliance.bool b
      | FLOAT f -> Compliance.number (`Float f)
      | INFINITY -> Compliance.number `Infinity
      | NEGINFINITY -> Compliance.number `Neginfinity
      | NAN -> Compliance.number `Nan
      | NULL -> Compliance.null
        (* LARGEINT is actually handled by the lexxer *)
      | LARGEINT s ->
        Compliance.number (`Float (float_of_string s))
      | EOF -> raise (Parse_error `Eof)
      | COMMA | COLON | AE | OE | LEX_ERROR _ | COMPLIANCE_ERROR _ -> parse_error tok
      | AS -> array_value ()
      | OS -> object_value ()
    end
    and value () = token_value (reader ())
    and array_value () = begin
      let state = ref `Value_or_end in
      let acc = ref [] in
      while !state <> `End do
        match !state with
        | `Value_or_end -> begin
            match reader () with
            | AE -> state := `End
            | tok -> acc := ((token_value tok)::!acc); state := `Comma_or_end
          end
        | `Comma_or_end -> begin
            match reader () with
            | AE -> state := `End
            | COMMA -> state := `Value
            | tok -> parse_error tok
          end
        | `Value -> 
          acc := ((value ())::!acc); state := `Comma_or_end
        | `End -> assert false
      done;
      Compliance.list (List.rev !acc)
    end
    and object_value () = begin
      let state = ref `Key_or_end in
      let acc = ref [] in
      let key = ref "" in
      while !state <> `End do
        match !state with
        | `Key_or_end -> begin
            match reader () with
            | OE -> state := `End
            | STRING s -> key := s; state := `Colon
            | tok -> parse_error tok
          end
        | `Colon -> begin
            match reader () with
            | COLON -> state := `Value
            | tok -> parse_error tok
          end
        | `Value -> 
          acc := ((!key, value ())::!acc); key := ""; state := `Comma_or_end
        | `Comma_or_end -> begin
            match reader () with
            | OE -> state := `End
            | COMMA -> state := `Key_value
            | tok -> parse_error tok
          end
        | `Key_value -> begin
            match reader () with
            | STRING s -> key := s; state := `Colon
            | tok -> parse_error tok
          end
        | `End -> assert false
      done;
      Compliance.assoc (List.rev !acc)
    end
    in
      value ()

  let lax ~reader = 
    try Ok (Some (json_value reader)) with
    | Parse_error `Eof -> Ok None
    | Parse_error (`Syntax_error err) -> Error err

  let ecma404 = lax
end
