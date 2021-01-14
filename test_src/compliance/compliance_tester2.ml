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


module CmdlineOptions = struct
  let tester command file alco_opts =
    Printf.printf "command: %s\nFile: %s\nAlcoTest: %s\n" command file (String.concat ", " alco_opts)

  open Cmdliner
  let alco_opts = Arg.(value & pos_all string [] & info [] ~docv:"-- [AlcoTest options]")
  let tfile = Arg.(required & opt (some file) None & info ["t"; "tests"] ~docv:"FILE" ~doc:"file to get test data from")

  let compliance_cmd f =
    let doc = "perform compliance checks with the various supported levels" in
    Term.(const f $ tfile $ alco_opts),
    Term.info "compliance" ~exits:Term.default_exits ~doc

  let validation_cmd gen validate =
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
    Term.info "validation" ~exits:Term.default_exits ~doc

  let default_cmd =
    let exits = Term.default_exits in
    Term.(ret (const (fun _ -> `Help (`Pager, None)) $ alco_opts)),
    Term.info "compliance_tester" ~version:"%%VERSION%%" ~exits

  let command = Arg.(required & pos 0 (some string) None & info [] ~docv:"[compliance|validation]")
  
  let cmd =
    Term.(const tester $ command $ tfile $ alco_opts),
    Term.info "compliance_tester" ~version:"%%VERSION%%" ~exits:Term.default_exits

end

let tester_compliance file alco_opts =
  Printf.printf "command: compliance\nFile: %s\nAlcoTest: %s\n" file (String.concat ", " alco_opts)

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
