module type IO = Io.IO

module type Parser = sig
  module IO : Io.IO
  module Compliance : Compliance.S

  val lax
    :  reader : (unit -> Tokens.token IO.t)
    -> (Compliance.json option, string) result
end

module Make (Compliance : Compliance.S) (IO : IO) : Parser = struct
  module IO = IO
  module Compliance = Compliance

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

  let token_error _tok = "unexpected *replace*"

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
      | AS -> array_value_start ()
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
        peek () >>= fun tok -> 
        match tok with
        | AE ->
          discard ()
          >>= fun () -> return (Compliance.list (List.rev (v::acc)))
        | COMMA -> array_values (v::acc)
        | tok -> fail (token_error tok)
    end
    in
    value ()

  let lax ~reader = Ok None

(*
  | OS; obj = object_fields; OE
    { Compliance.assoc obj }
  | AS; l = list_values; AE
    { Compliance.list l }

  | s = LARGEINT
    (* LARGEINT is actually handled by the lexxer *)
    { Compliance.number (`Float (float_of_string s)) }
  | NULL
    { Compliance.null }
  | INFINITY
    { Compliance.number `Infinity }
  | NEGINFINITY
    { Compliance.number `Neginfinity }
  | NAN
    { Compliance.number `Nan }
  | f = FLOAT
    { Compliance.number (`Float f) }
  | b = BOOL
    { Compliance.bool b }
  | s = STRING
    { Compliance.string s }
  | i = INT
    { Compliance.integer i }
*)

end

module Simple_parser = Make (Json_parse_types.Extended) (struct type 'a t = 'a let return v = v let (>>=) a f = f a end)

let () = Printf.printf "Hello!\n"
