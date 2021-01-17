let string_parse_std jsons =
  match Jsonxt.Extended.json_of_string jsons with
  | Ok _ -> true
  | Error _ -> false

let string_parse_stream jsons =
  let stream = Jsonxt.Extended_stream.json_stream_of_string jsons in
  let rec loop () =
    match Jsonxt.Extended_stream.decode_stream stream with
    | Error _ -> false
    | Ok None -> true
    | Ok Some _ -> loop ()
  in
  loop ()

let string_parse_monad jsons =
  let open Utils.IO in
  let iobuf = Utils.StringIO.create jsons in
  let reader buf len = Utils.StringIO.read iobuf buf len |> Utils.IO.return in
  let module JsonIO = Jsonxt.Extended_monad.Make(Utils.IO) in
  match result (JsonIO.read_json ~reader) with
  | Ok _ -> true
  | Error _ -> false
