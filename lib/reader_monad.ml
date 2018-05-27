module type Reader_monad = sig
  module IO
  type json

  val read_json : reader:(Bytes.t -> int -> int IO.t) -> (json, string) result IO.t
end

module Make
    (Lexxer : Compliant_lexxer_monad.Lex )
    (Parser : Parser_monad.Parser)
  : Reader_monad with type json = Parser.Compliance.json
= struct
  type json = Parser.Compliance.json
  type t = json

  let create_lexxer reader =
    let module Lexxer_internal = Lexxer.Make(struct
      include Lexxer.IO
      let read buf len = reader buf len
    end)
  in
  fun lexbuf -> Lexxer_internal.read lexbuf

  let read_json ~reader =
    let reader () = create_lexxer reader in
    match Parser.decode ~reader with
    | Ok None -> Error "empty string"
    | Ok (Some res) -> Ok res
    | Error s ->
      let loc = Lexxer_utils.error_pos_msg lexbuf in
        Error (Printf.sprintf "%s at %s" s loc)
end
