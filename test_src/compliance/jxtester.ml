module Utils = struct
  let die msg = 
    Printf.fprintf stderr "\nERROR: %s\n" msg;
    exit 255

  let load_file f =
    let ic = open_in f in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    Bytes.to_string s
end

module CmdlineOptions = struct
  let tester command file alco_opts =
    Printf.printf "command: %s\nFile: %s\nAlcoTest: %s\n" command file (String.concat ", " alco_opts)

  open Cmdliner
  let alco_opts = Arg.(value & pos_all string [] & info [] ~docv:"-- [AlcoTest options]")
  let tfile = Arg.(required & opt (some file) None & info ["t"; "tests"] ~docv:"FILE" ~doc:"file to get test data from")

  let compliance_cmd f =
    let doc = "perform compliance checks with the various supported levels" in
    let man = [
      `S Manpage.s_description;
      `P "run internal compliance testing as defined in -t FILE. This has the format:";
      `Pre "  <level> [pass|fail] <filename>";
      `P "Where the fields are defined as follows";
      `I ("level",
          "one of strict, basic, extended, yjbasic and yjsafe. These correspond)
           to the various compliance levels (yjbasic maps to Yojson.Basic etc)");
      `I ("[pass|fail]", "indicates the expected outcome");
      `I ("<filename>",
          "the file containing the test, it is assumed to be in the current directory,
           .json is automatically appended to the filename.")
    ]
    in
    Term.(const f $ tfile $ alco_opts),
    Term.info "compliance" ~exits:Term.default_exits ~doc ~man

  let validation_cmd gen validate =
    let man = [
      `S Manpage.s_description;
      `P "run the parser test suite, parsing and verifying each of the
          json strings defined in -t FILE. This is a tab seperated list
          of json and expected sexp in the format:";
      `Pre "  json <tab> sexp <tab> sexp_strem";
      `P "Where the fields are defined as follows";
      `I ("json", "json to parse");
      `I ("sexp", "is the expected sexp");
      `I ("sexp_stream", "is the expected sexp from the stream parser");
      `P "When in gen mode the sexps are ignored and a file suitable for
          using with run is output to stdout\n"
    ]
    in
    let gen_run subcmd tfile alco_opts =
      match subcmd with
      | "gen" -> gen tfile alco_opts; `Ok ()
      | "run" -> validate tfile alco_opts; `Ok ()
      | _ -> `Error (true, "expected run or gen")
    in
    let alco_opts = Arg.(value & pos_right 0 string [] & info [] ~docv:"-- [AlcoTest options]") in
    let doc = "run to run encode/decode, gen to generated validation data" in
    let subcmd = Arg.(required & pos 0 (some string) None & info [] ~docv:"[gen|run]" ~doc) in
    let doc = "perform decode and encode validation" in
    Term.(ret (const gen_run $ subcmd $ tfile $ alco_opts)),
    Term.info "validation" ~exits:Term.default_exits ~doc ~man

  let default_cmd =
    let exits = Term.default_exits in
    Term.(ret (const (fun _ -> `Help (`Pager, None)) $ alco_opts)),
    Term.info "jxtester" ~version:"%%VERSION%%" ~exits

  let command = Arg.(required & pos 0 (some string) None & info [] ~docv:"[compliance|validation]")
  
  let cmd =
    Term.(const tester $ command $ tfile $ alco_opts),
    Term.info "jxtester" ~version:"%%VERSION%%" ~exits:Term.default_exits

end


module ComplianceTests = struct
  let read_test_data inc =
    let l = input_line inc in
    let p = String.split_on_char ' ' l |> List.filter (fun v -> not (String.equal "" v)) in
    let (level, passfail, filename) = match p with
      | lv::pf::fn::[] -> (lv, pf, fn)
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
    (level, passfail, filename)

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

  let tester f level filename passfail () =
    let slevel = level_to_string level in
    let msg = slevel ^ " " ^ filename in
    Alcotest.(check string) msg passfail (f level filename) 

  let gen_tests filename =
    let inc = try open_in filename with | Sys_error err -> Utils.die err in
    let rec loop str file =
      match read_test_data inc with
      | level, passfail, filename ->
        let msg = filename ^ " " ^ (level_to_string level) in
        loop ((Alcotest.test_case msg `Quick (tester string_parse_test level filename passfail))::str)
             ((Alcotest.test_case msg `Quick (tester file_parse_test level filename passfail))::file)
      | exception End_of_file -> (str, file)
    in
    let str_t, file_t = loop [] [] in [ "string", (List.rev str_t); "file", (List.rev file_t) ]

  let run_tests filename alco_opts =
    let argv = Array.of_list ("compliance"::alco_opts) in
    Alcotest.run ~argv "Compliance" (gen_tests filename)
end


module ValidationTests = struct
  module JsonSexp = struct
    open Core_kernel
    type json = [
         `Null | `Bool of bool | `Int of int | `Intlit of string | `Float of float
      | `Floatlit of string | `String of string | `Stringlit of string
      | `Assoc of (string * json) list | `List of json list | `Tuple of json list
      | `Variant of (string * json option) ] [@@deriving sexp]
  end

  module JsonStreamSexp = struct
    open Core_kernel
    type json_stream = [
        `Null | `Bool of bool | `Int of int | `Intlit of string | `Float of float
      | `Floatlit of string | `String of string | `Stringlit of string
      | `As | `Ae | `Os | `Oe | `Ts | `Te | `Vs | `Ve | `Name of string
      | `Infinity | `Neg_infinity | `Nan ] list [@@deriving sexp]
  end

  let read_json_sexp inc =
    let l = input_line inc in
    let p = String.split_on_char '\t' l in
    let (jsons, sexps, sexps_json_stream) = match p with
      | jv::sv::ssv::[] -> (jv, sv, ssv)
      | jv::[] -> (jv, "", "")
      | _ -> Utils.die ("invalid test line: " ^ l)
    in
    (jsons, sexps, sexps_json_stream)

  let output_validation_config jsons json json_stream =
    Printf.printf "%s\t%s\t%s\n"
      jsons
      (JsonSexp.sexp_of_json json |> Core_kernel.Sexp.to_string)
      (JsonStreamSexp.sexp_of_json_stream json_stream |> Core_kernel.Sexp.to_string)

  let get_json_stream jsons =
    let stream = Jsonxt.Extended_stream.json_stream_of_string jsons in
    let rec loop res =
      match Jsonxt.Extended_stream.decode_stream stream with
      | Error err -> Error err
      | Ok None -> Ok (List.rev res)
      | Ok Some tok -> loop (tok::res)
    in
    loop []
  
  let parse_json jsons =
    let json_result = Jsonxt.Extended.json_of_string jsons in
    let json_stream_result = get_json_stream jsons in
    let json = 
      match json_result with
      | Ok json -> json
      | Error err -> Printf.sprintf "failed to parse \"%s\": %s" jsons err |> Utils.die
    in
    let json_stream = 
      match json_stream_result with
      | Ok json_stream -> json_stream
      | Error err -> Printf.sprintf "failed to parse stream \"%s\": %s" jsons err |> Utils.die
    in
    (json, json_stream)

  let gen_config filename _alco_opts =
    let inc = try open_in filename with | Sys_error err -> Utils.die err in
    let rec loop () =
      match read_json_sexp inc with
      | jsons, _sexps, _sexps_json_stream ->
        let json, json_stream = parse_json jsons in
        output_validation_config jsons json json_stream;
        loop ()
      | exception End_of_file -> ()
    in loop ()

  let get_json_stream jsons =
    let stream = Jsonxt.Extended_stream.json_stream_of_string jsons in
    let rec loop res =
      match Jsonxt.Extended_stream.decode_stream stream with
      | Error err -> Error err
      | Ok None -> Ok (List.rev res)
      | Ok Some tok -> loop (tok::res)
    in
    loop []

  let json_stream_to_string stream =
    let buf = Buffer.create 100 in
    let add_char c = Buffer.add_char buf c in
    let add_string s = Buffer.add_string buf s in
    let encoder = Jsonxt.Extended_stream.create_encoder ~add_char ~add_string in
    let encode () =
      List.iter (fun v -> Jsonxt.Extended_stream.encode_stream_exn encoder v) stream;
      Buffer.contents buf
    in
    try Ok (encode ()) with
    | Failure err -> Error err

  let sexp =
    let pp ppf v = Fmt.pf ppf "%s" (Core_kernel.Sexp.to_string v) in
    let sexp_eq a b = match Core_kernel.Sexp.compare a b with | 0 -> true | _ -> false in
    Alcotest.testable pp sexp_eq

  let string_parse_test jsons sexps () =
    let jsonsexp = 
      match Jsonxt.Extended.json_of_string jsons with
      | Ok json -> JsonSexp.sexp_of_json json
      | Error err -> Core_kernel.Sexp.Atom (Printf.sprintf "Failed to parse '%s': %s" jsons err)
    in
    let sexpv = Core_kernel.Sexp.of_string sexps in
    Alcotest.(check sexp) jsons sexpv jsonsexp 
    
  let gen_tests filename =
    let inc = try open_in filename with | Sys_error err -> Utils.die err in
    let rec loop std stream =
      match read_json_sexp inc with
      | jsons, sexps, _sexps_json_stream ->
        let msg = jsons in
        loop ((Alcotest.test_case msg `Quick (string_parse_test jsons sexps))::std)
             stream
      | exception End_of_file -> (std, stream)
    in
    let std_t, stream_t = loop [] [] in [ "standard", (List.rev std_t); "stream", (List.rev stream_t) ]

  let run_tests filename alco_opts =
    let argv = Array.of_list ("compliance"::alco_opts) in
    Alcotest.run ~argv "Validation" (gen_tests filename)
end

let tester_validation_run file alco_opts =
  Printf.printf "command: validate run\nFile: %s\nAlcoTest: %s\n" file (String.concat ", " alco_opts)

let tester_validation_gen file alco_opts =
  Printf.printf "command: validate gen\nFile: %s\nAlcoTest: %s\n" file (String.concat ", " alco_opts)


(* let () = Cmdliner.Term.(exit @@ eval CmdlineOptions.cmd) *)
let cmds = [
    CmdlineOptions.compliance_cmd ComplianceTests.run_tests
  ; CmdlineOptions.validation_cmd ValidationTests.gen_config ValidationTests.run_tests
  ]
let () = Cmdliner.Term.(exit @@ eval_choice CmdlineOptions.default_cmd cmds)
