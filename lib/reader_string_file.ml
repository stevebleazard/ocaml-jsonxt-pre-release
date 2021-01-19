module type Reader_string_file = sig
  type json

  exception Finally of exn * exn

  val json_of_string : string -> (json, string) result
  val json_of_string_exn : string -> json
  val json_of_file : string -> (json, string) result
  val json_of_file_exn : string -> json
  val json_of_channel : in_channel -> (json, string) result
  val json_of_channel_exn : in_channel -> json
  val json_of_function : (bytes -> int -> int) -> (json, string) result
  val json_of_function_exn : (bytes -> int -> int) -> json
  val of_string : string -> json
  val of_file : string -> json
  val of_channel : in_channel -> json
  val of_function : (bytes -> int -> int) -> json
  val stream_from_string : string -> json Stream.t
  val stream_from_channel : ?fin:(unit -> unit) -> in_channel -> json Stream.t
  val stream_from_file : string -> json Stream.t
  val stream_from_function : (bytes -> int -> int) -> json Stream.t
end

module Make (Lexxer : Compliant_lexxer.Lex ) (Parser : Parser.Parser) : Reader_string_file
  with type json = Parser.Compliance.json
= struct
  type json = Parser.Compliance.json

  exception Finally of exn * exn

  let read_json ~lexbuf =
    let reader () = Lexxer.read lexbuf in
    match Parser.decode ~reader with
    | Ok None -> Error "empty input"
    | Ok (Some res) -> begin
      match reader () with
      | EOF -> Ok res
      | exception Lexxer_utils.Lex_error err -> Error err
      | tok -> Error ("junk after end of JSON value: " ^ (Token_utils.token_to_string tok))
      end
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

  let json_of_function f =
    let lexbuf = Lexing.from_function f in
    read_json ~lexbuf

  let json_of_function_exn f =
    match json_of_function f with
    | Ok res -> res
    | Error s -> raise (Failure s)

  let of_file = json_of_file_exn
  let of_channel = json_of_channel_exn
  let of_function = json_of_function_exn

  let read_json_stream ~fin ~lexbuf =
    let reader () = Lexxer.read lexbuf in
    let f _i =
      match Parser.decode ~reader with
      | Ok None -> fin (); None
      | Ok (Some res) -> Some res
      | Error err ->
        let fexn_ = match fin () with exception exn_ -> Some exn_ | () -> None in
        let loc = Lexxer_utils.error_pos_msg lexbuf in
        let msg = Printf.sprintf "%s at %s" err loc in
        match fexn_ with
        | None -> raise (Failure msg)
        | Some fexn_ -> raise (Finally ((Failure msg), fexn_))
    in
    Stream.from f

  let stream_from_string s =
    let lexbuf = Lexing.from_string s in
    read_json_stream ~fin:(fun () -> ()) ~lexbuf

  let stream_from_channel ?(fin = fun () -> ()) inc =
    let lexbuf = Lexing.from_channel inc in
    read_json_stream ~fin ~lexbuf

  let stream_from_function f =
    let lexbuf = Lexing.from_function f in
    read_json_stream ~fin:(fun () -> ()) ~lexbuf

  let stream_from_file filename =
    let inc = open_in filename in
    stream_from_channel ~fin:(fun () -> close_in inc) inc

end
