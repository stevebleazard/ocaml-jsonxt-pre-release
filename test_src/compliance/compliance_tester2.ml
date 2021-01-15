(*
module To_test = struct
  let lowercase = String.lowercase_ascii
  let capitalize = String.capitalize_ascii
  let str_concat = String.concat ""
  let list_concat = List.append
end

(* The tests *)
let test_lowercase () =
  Alcotest.(check string) "same string" "hello!" (To_test.lowercase "hELLO!")

let test_capitalize () =
  Alcotest.(check string) "same string" "World." (To_test.capitalize "world.")

let test_str_concat () =
  Alcotest.(check string) "same string" "foobar" (To_test.str_concat ["foo"; "bar"])

let test_list_concat () =
  Alcotest.(check (list int)) "same lists" [1; 2; 3] (To_test.list_concat [1] [2; 3])

(* Run it *)
let () =
  let open Alcotest in
  run "Utils" [
      "string-case", [
          test_case "Lower case"     `Quick test_lowercase;
          test_case "Capitalization" `Quick test_capitalize;
        ];
      "string-concat", [ test_case "String mashing" `Quick test_str_concat  ];
      "list-concat",   [ test_case "List mashing"   `Slow  test_list_concat ];
    ]
*)

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
    Term.info "jxttester" ~version:"%%VERSION%%" ~exits

  let command = Arg.(required & pos 0 (some string) None & info [] ~docv:"[compliance|validation]")
  
  let cmd =
    Term.(const tester $ command $ tfile $ alco_opts),
    Term.info "jxttester" ~version:"%%VERSION%%" ~exits:Term.default_exits

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
    let str_t, file_t = loop [] [] in [ "string", str_t; "file", file_t ]

  let run_tests filename alco_opts =
    let argv = Array.of_list ("compliance"::alco_opts) in
    Alcotest.run ~argv "Compliance" (gen_tests filename)
end

let tester_compliance file alco_opts =
  Printf.printf "command: compliance\nFile: %s\nAlcoTest: %s\n" file (String.concat ", " alco_opts);
  ComplianceTests.run_tests file alco_opts

let tester_validation_run file alco_opts =
  Printf.printf "command: validate run\nFile: %s\nAlcoTest: %s\n" file (String.concat ", " alco_opts)

let tester_validation_gen file alco_opts =
  Printf.printf "command: validate gen\nFile: %s\nAlcoTest: %s\n" file (String.concat ", " alco_opts)


(* let () = Cmdliner.Term.(exit @@ eval CmdlineOptions.cmd) *)
let cmds = [
    CmdlineOptions.compliance_cmd tester_compliance
  ; CmdlineOptions.validation_cmd tester_validation_gen tester_validation_run
  ]
let () = Cmdliner.Term.(exit @@ eval_choice CmdlineOptions.default_cmd cmds)
