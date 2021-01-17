let read_test_data inc =
  let l = input_line inc in
  let p = String.split_on_char ' ' l |> List.filter (fun v -> not (String.equal "" v)) in
  let (level, passfail, filename, bits) = match p with
    | lv::pf::fn::"32"::[] -> (lv, pf, fn, "32")
    | lv::pf::fn::"64"::[] -> (lv, pf, fn, "64")
    | lv::pf::fn::"all"::[] -> (lv, pf, fn, "all")
    | lv::pf::fn::[] -> (lv, pf, fn, "all")
    | _ -> Utils.die ("invalid test line: " ^ l)
  in
  let level = match level with
    | "strict"   -> `Strict
    | "basic"    -> `Basic
    | "extended" -> `Extended
    | "yjsafe"   -> `Yojson_safe
    | "yjbasic"  -> `Yojson_basic
    | _ -> Utils.die ("invalid test line, first column invalid level: " ^ l)
  in
  let passfail = match passfail with
    | "pass" | "fail" as v -> v
    | _ -> Utils.die ("invalid test line, second column must be pass or fail: " ^ l)
  in
  (level, passfail, filename, bits)

let of_error f a = match f a with Ok _ -> "pass" | Error _ -> "fail"

let level_to_string = function
  | `Strict -> "strict"
  | `Basic -> "basic"
  | `Extended -> "extended"
  | `Yojson_safe -> "yjsafe"
  | `Yojson_basic -> "yjbasic"

let string_parse_test level filename =
  let txt = try Utils.load_file (filename ^ ".json") with Sys_error err -> Utils.die err in
  let string_parser = match level with
    | `Strict       -> of_error Jsonxt.Strict.json_of_string
    | `Basic        -> of_error Jsonxt.Basic.json_of_string
    | `Extended     -> of_error Jsonxt.Extended.json_of_string
    | `Yojson_basic -> of_error Jsonxt.Yojson.Basic.json_of_string
    | `Yojson_safe  -> of_error Jsonxt.Yojson.Safe.json_of_string
  in 
  string_parser txt

let file_parse_test level filename =
  let file_parser = match level with
    | `Strict       -> of_error Jsonxt.Strict.json_of_file
    | `Basic        -> of_error Jsonxt.Basic.json_of_file
    | `Extended     -> of_error Jsonxt.Extended.json_of_file
    | `Yojson_basic -> of_error Jsonxt.Yojson.Basic.json_of_file
    | `Yojson_safe  -> of_error Jsonxt.Yojson.Safe.json_of_file
  in 
  file_parser (filename ^ ".json")

let monad_parser_test level filename =
  let open Utils.IO in
  let txt = try Utils.load_file (filename ^ ".json") with Sys_error err -> Utils.die err in
  let iobuf = Utils.StringIO.create txt in
  let reader buf len = Utils.StringIO.read iobuf buf len |> Utils.IO.return in
  let of_error res = match res with Ok _ -> return "pass" | Error _ -> return "fail" in
  let module JsonIOStrict = Jsonxt.Strict_monad.Make(Utils.IO) in
  let module JsonIOBasic = Jsonxt.Basic_monad.Make(Utils.IO) in
  let module JsonIOExtended = Jsonxt.Extended_monad.Make(Utils.IO) in
  let result = match level with
    | `Strict       -> JsonIOStrict.read_json ~reader >>= of_error
    | `Basic        -> JsonIOBasic.read_json ~reader >>= of_error
    | `Extended     -> JsonIOExtended.read_json ~reader >>= of_error
    | `Yojson_basic -> return "pass"
    | `Yojson_safe  -> return "pass"
  in
  Utils.IO.result result

let tester f level filename passfail () =
  let slevel = level_to_string level in
  let msg = slevel ^ " " ^ filename in
  Alcotest.(check string) msg passfail (f level filename) 

let gen_tests filename =
  let inc = try open_in filename with | Sys_error err -> Utils.die err in
  let rec loop str file =
    match read_test_data inc with
    | level, passfail, filename, bits -> begin
      let msg = filename ^ " " ^ (level_to_string level) in
      let stest = Alcotest.test_case msg `Quick (tester string_parse_test level filename passfail) in
      let ftest = Alcotest.test_case msg `Quick (tester file_parse_test level filename passfail) in
      match bits with
      | "64" when Utils.int_bits = 32 -> loop str file
      | "32" when Utils.int_bits = 64 -> loop str file
      | _ -> loop (stest::str) (ftest::file)
      end
    | exception End_of_file -> (str, file)
  in
  let str_t, file_t = loop [] [] in [ "string", (List.rev str_t); "file", (List.rev file_t) ]

let run_tests filename alco_opts =
  let argv = Array.of_list ("compliance"::alco_opts) in
  Alcotest.run ~argv "Compliance" (gen_tests filename)
