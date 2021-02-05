let string_parse_std jsons =
  match Jsonxt.Strict.json_of_string jsons with
  | Ok _ -> `Pass
  | Error _ -> `Fail

let string_parse_stream jsons =
  let stream = Jsonxt.Strict_stream.json_stream_of_string jsons in
  let rec loop () =
    match Jsonxt.Strict_stream.decode_stream stream with
    | Error _ -> `Fail
    | Ok None -> `Pass
    | Ok Some _ -> loop ()
  in
  loop ()

let string_parse_monad jsons =
  let open Utils.IO in
  let iobuf = Utils.StringIO.create jsons in
  let reader buf len = Utils.StringIO.read iobuf buf len |> Utils.IO.return in
  let module JsonIO = Jsonxt.Strict_monad.Make(Utils.IO) in
  match result (JsonIO.read_json ~reader ()) with
  | Ok _ -> `Pass
  | Error _ -> `Fail

let filename_to_success filename =
  let name = Filename.basename filename in
  match String.get name 0 with
  | 'y' | 'Y' -> `Pass
  | 'n' | 'N' -> `Fail
  | 'i' | 'I' -> `Undefined
  | _         -> `Undefined

let result_to_string = function
  | `Pass -> "pass"
  | `Fail -> "fail"
  | `Undefined -> "undef"

let result_to_report expected actual =
  match expected, actual with
  | `Pass, `Pass
  | `Fail, `Fail -> "pass"
  | `Pass, `Fail
  | `Fail, `Pass -> "fail"
  | `Undefined, `Pass -> "OKpass"
  | `Undefined, `Fail -> "OKfail"

let charfill text tolen chr =
  let textlen= String.length text in
  if textlen >= tolen then text
  else begin
    let chars = String.make (tolen - textlen) chr in
    text ^ chars
  end

let report filename actual =
  let expected = filename_to_success filename in
  let report = result_to_report expected actual in
  let report = charfill report 8 ' ' in
    Printf.printf "  %s  %s\n" report filename

let test_suite_std_file filename =
  let jsons = try Utils.load_file filename with Sys_error err -> Utils.die err in
  let actual = string_parse_std jsons in
  report filename actual

let test_suite_std files =
  Printf.printf "Standard parser\n";
  List.iter test_suite_std_file files

let test_suite_stream_file filename =
  let jsons = try Utils.load_file filename with Sys_error err -> Utils.die err in
  let actual = string_parse_stream jsons in
  report filename actual

let test_suite_stream files =
  Printf.printf "Stream parser\n";
  List.iter test_suite_stream_file files

let test_suite_monad_file filename =
  let jsons = try Utils.load_file filename with Sys_error err -> Utils.die err in
  let actual = string_parse_monad jsons in
  report filename actual

let test_suite_monad files =
  Printf.printf "Monad parser\n";
  List.iter test_suite_monad_file files
