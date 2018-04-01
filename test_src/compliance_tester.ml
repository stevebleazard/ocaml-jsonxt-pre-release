open Printf

let help err =
  begin
    match err with
    | Some err -> printf "ERROR: %s\n\n" err
    | None -> ()
  end;
  printf "compliance_tester [help|suite|internal]\n";
  exit 0

let run_test_suite _idx = ()
let run_internal_tests _idx = ()

let () =
  if Array.length Sys.argv < 2 then help (Some "expected sub-test to run");
  match Sys.argv.(1) with
  | "help" -> help None
  | "suite" -> run_test_suite 2
  | "internal" -> run_internal_tests 2
  | _ -> help (Some ("unknown command " ^ Sys.argv.(1)))
