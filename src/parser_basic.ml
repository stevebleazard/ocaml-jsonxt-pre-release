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

  module LA = struct
    let labuf = ref None
    
    let read reader () = 
      match !labuf with
      | Some tok -> labuf := None; tok
      | None -> reader ()

    let peek reader () =
      match !labuf with
      | None -> let tok = reader () in labuf := Some tok; tok
      | Some tok -> tok
  end

  let token_error tok =
    let open Tokens in
    let err = match tok with
      | STRING s -> "unexpected string '" ^ s ^ "')"
      | OS -> "unexpected '{'"
      | OE -> "unexpected '}'"
      | NULL -> "unexpected null value"
      | NEGINFINITY -> "unexpected negative infinity"
      | NAN -> "unexpected Not-a-Number"
      | LEX_ERROR s -> "unexpected char '" ^ s ^ "'"
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

  let json_value ~reader = 
    let open Tokens in
    let read = LA.read reader in
    let peek = LA.peek reader in
    let discard () = let _ = read () in () in

    let rec value () = begin
      let tok = read ()  in
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
      | COMMA | COLON | AE | OE | LEX_ERROR _ | COMPLIANCE_ERROR _ ->
        raise (Parse_error (token_error tok))
      | AS -> array_value_start ()
      | OS -> object_value_start ()
    end
    and array_value_start () = begin
      let tok = peek () in
      match tok with
      | AE -> let () = discard () in Compliance.list []
      | _ -> array_values []
    end
    and array_values acc = begin
      let v = value () in
      let tok = read () in
        match tok with
        | AE -> Compliance.list (List.rev (v::acc))
        | COMMA -> array_values (v::acc)
        | tok -> raise (Parse_error (token_error tok))
    end
    and object_value_start () = begin
      let tok = peek () in
      match tok with
      | OE -> let () = discard () in Compliance.assoc []
      | _ -> object_values []
    end
    and object_values acc = begin
      let v = key_colon_value () in
      let tok = read () in
        match tok with
        | OE -> Compliance.assoc (List.rev (v::acc))
        | COMMA -> object_values (v::acc)
        | tok -> raise (Parse_error (token_error tok))
    end
    and key_colon_value () = begin
      match read () with
        | STRING k -> begin
          match read () with
            | COLON -> (k, value ())
            | tok ->  raise (Parse_error (token_error tok))
          end
        | tok ->  raise (Parse_error (token_error tok))
    end
    in
    value ()

  let lax ~reader = 
    try Ok (Some (json_value reader)) with
    | Parse_error `Eof -> Ok None
    | Parse_error (`Syntax_error err) -> Error err

  let ecma404 = lax
end
