module type Reader_monad = sig
  module IO : Io.IO
  type json

  val read_json : reader:(Bytes.t -> int -> int IO.t) -> (json, string) result IO.t
end

module Make
    (Parser : Parser_monad.Parser)
  : Reader_monad with type json = Parser.Compliance.json and module IO := Parser.IO
= struct
  type json = Parser.Compliance.json
  type t = json

  open Parser.IO

  let create_lex_reader reader =
    let module Lexxer = Compliant_lexxer_monad.Make (Parser.Compliance)
        (struct
          module IO = Parser.IO
          include IO
          let read buf len = reader buf len
        end)
    in
    fun lexbuf -> Lexxer.read lexbuf

  let read_json ~reader =
    let lexbuf = Lexutils.create_lexbuf () in
    let lex_reader = create_lex_reader reader in
    let reader () = 
      lex_reader lexbuf
      >>= function
      | Ok tok -> return tok
      | Error err -> return (Tokens.LEX_ERROR err)
    in
    Parser.decode ~reader
    >>= function
    | Ok None -> return (Error "empty string")
    | Ok (Some res) -> return (Ok res)
    | Error s ->
      let loc = Lexxer_utils.error_pos_msg lexbuf in
        return (Error (Printf.sprintf "%s at %s" s loc))
end
