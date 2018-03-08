module Lexxer = Compliant_lexxer.Make(Json_parse_types.Basic)
module Parser = Parser.Make(Json_parse_types.Basic)

type json = Json.Basic.json
type t = json

let json_of_string ?strict s =
  let parse =
    match strict with
    | Some true -> Parser.lax  (* CR sbleazard: fix *)
    | _ -> Parser.lax
  in
  let lexbuf = Lexing.from_string s in
  let reader () = Lexxer.read lexbuf in
  match parse ~reader with
  | Ok None -> Error "empty string"
  | Ok (Some res) -> Ok res
  | Error s ->
    let loc = Lexxer_utils.error_pos_msg lexbuf in
      Error (Printf.sprintf "%s at %s\n" s loc)

let json_of_string_exn ?strict s =
  match json_of_string ?strict s with
  | Ok res -> res
  | Error s -> raise (Failure s)

let of_string s = json_of_string_exn s 
