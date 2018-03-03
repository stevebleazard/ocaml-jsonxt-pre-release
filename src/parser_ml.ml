module type IO = Io.IO

module type Parser = sig
  module IO : Io.IO
  module Compliance : Compliance.S

  val lax
    :  reader : (unit -> Tokens.token IO.t)
    -> (Compliance.json option, string) result IO.t
end

module Make (Compliance : Compliance.S) (IO : IO) : Parser
  with module IO := IO
   and module Compliance := Compliance
= struct

  open IO

  module LA = struct
    let labuf = ref None
    
    let read reader () = 
      match !labuf with
      | Some tok -> labuf := None; return tok
      | None -> reader ()

    let peek reader () =
      match !labuf with
      | None -> reader () >>= fun tok -> labuf := Some tok; return tok
      | Some tok -> return tok
  end

  module Error_or = struct
    let return v = IO.return (Ok v)
    let fail err = IO.return (Error err)

    let (>>=?) a f =
      a >>= fun a ->
      match a with
      | Ok a -> f a
      | Error err -> IO.return (Error err)
  end

  open Error_or

  (* CR sbleazard: fix *)
  let token_error tok =
    let open Tokens in
    let err = match tok with
      | STRING s -> "STRING(" ^ s ^ ")"
      | OS -> "OS"
      | OE -> "OE"
      | NULL -> "NULL"
      | NEGINFINITY -> "NEGINFINITY"
      | NAN -> "NAN"
      | LEX_ERROR s -> "LEX_ERROR(" ^ s ^ ")"
      | LARGEINT s -> "LARGEINT(" ^ s ^ ")"
      | INT i -> "INT(" ^ (string_of_int i) ^ ")"
      | INFINITY -> "INFINITY"
      | FLOAT f -> "FLOAT" ^ (string_of_float f) ^ ")"
      | EOF -> "EOF"
      | COMPLIANCE_ERROR s -> "COMPLIANCE(" ^ s ^ ")"
      | COMMA -> "COMMA"
      | COLON -> "COLON"
      | BOOL b -> "BOOL(" ^ (if b then "true" else "false") ^ ")"
      | AS -> "AS"
      | AE -> "AE"
    in
      `Syntax_error ("Unexpected " ^ err)

  let json_value ~reader = 
    let open Tokens in
    let read = LA.read reader in
    let peek = LA.peek reader in
    let discard () = read () >>= fun _ -> IO.return () in

    let rec value () = begin
      read () >>= fun tok ->
      match tok with
      | INT i -> return (Compliance.integer i)
      | STRING s -> return (Compliance.string s)
      | BOOL b -> return (Compliance.bool b)
      | FLOAT f -> return (Compliance.number (`Float f))
      | INFINITY -> return (Compliance.number `Infinity)
      | NEGINFINITY -> return (Compliance.number `Neginfinity)
      | NAN -> return (Compliance.number `Nan)
      | NULL -> return (Compliance.null)
        (* LARGEINT is actually handled by the lexxer *)
      | LARGEINT s ->
        return (Compliance.number (`Float (float_of_string s)))
      | EOF -> fail `Eof
      | COMMA | COLON | AE | OE | LEX_ERROR _ | COMPLIANCE_ERROR _ ->
        fail (token_error tok)
      | AS -> array_value_start ()
      | OS -> object_value_start ()
    end
    and array_value_start () = begin
      peek () >>= fun tok ->
      match tok with
      | AE -> discard () >>= fun () -> return (Compliance.list [])
      | _ -> array_values []
    end
    and array_values acc = begin
      value ()
      >>=? fun v ->
        read () >>= fun tok -> 
        match tok with
        | AE -> return (Compliance.list (List.rev (v::acc)))
        | COMMA -> array_values (v::acc)
        | tok -> fail (token_error tok)
    end
    and object_value_start () = begin
      peek () >>= fun tok ->
      match tok with
      | OE -> discard () >>= fun () -> return (Compliance.assoc [])
      | _ -> object_values []
    end
    and object_values acc = begin
      key_colon_value ()
      >>=? fun v ->
        read () >>= fun tok -> 
        match tok with
        | OE -> return (Compliance.assoc (List.rev (v::acc)))
        | COMMA -> object_values (v::acc)
        | tok -> fail (token_error tok)
    end
    and key_colon_value () = begin
      read ()
      >>= function
        | STRING k -> begin
          read ()
          >>= function
            | COLON -> begin value () >>=? fun v -> return (k, v) end
            | tok ->  fail (token_error tok)
          end
        | tok ->  fail (token_error tok)
    end
    in
    value ()

  let lax ~reader = 
    json_value reader
    >>= function
      | Ok res -> return (Some res)
      | Error `Eof -> return None
      | Error (`Syntax_error err) -> fail err

end
