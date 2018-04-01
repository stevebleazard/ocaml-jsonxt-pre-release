open Printf

let help_error err = 
  match err with
  | Some err -> printf "ERROR: %s\n\n" err
  | None -> ()

let help err =
  help_error err;
  printf "compliance_tester [help|suite|internal]\n";
  exit 0

let help_internal err =
  help_error err;
  printf "
   compliance_tester internal [help|run <file>]

   run internal compliance testing as defined in <file>. This has the format
     <level> [pass|fail] <filename>

   where
     <level>
       one of strict, basic, extended, yjbasic and yjsafe. These correspond
       to the various compliance levels (yjbasic maps to Yojson.Basic etc)
     [pass|fail]
       indicates the expected outcome
     <filename>
       the file containing the test, it is assumed to be in the current directory.
       
   Note that any number of spaces can seperate the fields.\n"

let run_internal_tests idx =
  help_internal None
  
let run_test_suite _idx = ()


let () =
  if Array.length Sys.argv < 2 then help (Some "expected sub-test to run");
  match Sys.argv.(1) with
  | "help" -> help None
  | "suite" -> run_test_suite 2
  | "internal" -> run_internal_tests 2
  | _ -> help (Some ("unknown command " ^ Sys.argv.(1)))
