module type Json_string_file = sig
  type json

  val json_of_string : string -> (json, string) result
  val json_of_string_exn : string -> json
  val json_of_file : string -> (json, string) result
  val json_of_file_exn : string -> json
  val of_string : string -> json
end

module Make (Lexxer : Compliant_lexxer.Lex ) (Parser : Parser.Parser) : Json_string_file
  with type json = Parser.Compliance.json
= struct
  type json = Parser.Compliance.json
  type t = json

  let read_json ~lexbuf =
    let reader () = Lexxer.read lexbuf in
    match Parser.decode ~reader with
    | Ok None -> Error "empty string"
    | Ok (Some res) -> Ok res
    | Error s ->
      let loc = Lexxer_utils.error_pos_msg lexbuf in
        Error (Printf.sprintf "%s at %s\n" s loc)

  let json_of_string s =
    let lexbuf = Lexing.from_string s in
    read_json ~lexbuf

  let json_of_string_exn s =
    match json_of_string s with
    | Ok res -> res
    | Error s -> raise (Failure s)

  let of_string s = json_of_string_exn s 

  let json_of_file filename =
    try begin
      let inc = open_in filename in
      let lexbuf = Lexing.from_channel inc in
      read_json ~lexbuf
    end
    with Sys_error err -> Error err

  let json_of_file_exn filename =
    match json_of_string filename with
    | Ok res -> res
    | Error s -> raise (Failure s)

end
