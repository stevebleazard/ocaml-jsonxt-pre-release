open Lexing

module Basic_lexxer = Compliant_lexxer.Make(Json_parse_types.Basic)
module Basic_parser = Parser.Make(Json_parse_types.Basic)

type json = Json.Basic.json
type t = json

let json_of_string ?strict s =
  let parse =
    match strict with
    | Some true -> Basic_parser.lax  (* CR sbleazard: fix *)
    | _ ->         Basic_parser.lax
  in
  let lexbuf = Lexing.from_string s in
  let reader () = Basic_lexxer.read lexbuf in
  match parse ~reader with
  | Ok None as res -> res
  | Ok (Some json) as res -> res
  | Error s ->
    let loc = Lexxer_utils.error_pos_msg lexbuf in
      Error (Printf.sprintf "%s at %s\n" s loc)

let of_string s = json_of_string s
