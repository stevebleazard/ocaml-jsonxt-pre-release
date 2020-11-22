module type Reader_string_file = sig
  type json

  val json_of_string : string -> (json, string) result
  val json_of_string_exn : string -> json
  val json_of_file : string -> (json, string) result
  val json_of_file_exn : string -> json
  val json_of_channel : in_channel -> (json, string) result
  val json_of_channel_exn : in_channel -> json
  val of_string : string -> json
  val of_file : string -> json
  val of_channel : in_channel -> json
end

module Make (Lexxer : Compliant_lexxer.Lex ) (Parser : Parser.Parser) : Reader_string_file
  with type json = Parser.Compliance.json
= struct
  type json = Parser.Compliance.json

  let read_json ~lexbuf =
    let reader () = Lexxer.read lexbuf in
    match Parser.decode ~reader with
    | Ok None -> Error "empty string"
    | Ok (Some res) -> Ok res
    | Error s ->
      let loc = Lexxer_utils.error_pos_msg lexbuf in
        Error (Printf.sprintf "%s at %s" s loc)

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
      let res = read_json ~lexbuf in
        close_in inc;
        res
    end
    with Sys_error err -> Error err

  let json_of_file_exn filename =
    match json_of_file filename with
    | Ok res -> res
    | Error s -> raise (Failure s)

  let json_of_channel inc =
    let lexbuf = Lexing.from_channel inc in
    read_json ~lexbuf

  let json_of_channel_exn inc =
    match json_of_channel inc with
    | Ok res -> res
    | Error s -> raise (Failure s)
  
  let of_file = json_of_file_exn
  let of_channel = json_of_channel_exn

end
