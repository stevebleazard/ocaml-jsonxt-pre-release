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
  printf "\
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
       
   Note that any number of spaces can seperate the fields.\n";
  exit 0

let execute_internal_tests inc =
  let read_params () =
    let l = input_line inc in
    let p = String.split_on_char ' ' l |> List.filter (fun v -> not (String.equal "" v)) in
    let (level, passfail, filename) = match p with
      | lv::pf::fn::[] -> (lv, pf, fn)
      | _ -> help_internal (Some ("invalid test line: " ^ l));
    in
    let _:unit = match level with
      | "strict" | "basic" | "extended" | "yjsafe" | "yjbasic"  -> ()
      | _ -> help_internal (Some ("invalid test line, first column invalid level: " ^ l))
    in
    let _:unit = match passfail with
      | "pass" | "fail" -> ()
      | _ -> help_internal (Some ("invalid test line, second column must be pass or fail: " ^ l))
    in
    (level, passfail, filename)
  in
  let rec run () = 
    let (_level, _passfail, _filename) = read_params () in
    run ()
  in
  try run () with
  | End_of_file -> ()



let run_internal_tests idx =
  let args = Array.length Sys.argv - idx in
  if args <= 0 then help_internal (Some "expected run or help");

  match Sys.argv.(idx) with
  | "help" -> help_internal None
  | "run" -> begin
    if args < 2 then help_internal (Some "expected filename of test schedule");
    let inf =
      try open_in Sys.argv.(idx + 1) with
      | _ -> help (Some ("failed to open " ^ Sys.argv.(idx + 1)))
    in
      execute_internal_tests inf;
      close_in inf
    end
  | cmd -> help_internal (Some ("unknown internal command " ^ cmd))
  
let run_test_suite _idx = ()


let () =
  if Array.length Sys.argv < 2 then help (Some "expected sub-test to run");
  match Sys.argv.(1) with
  | "help" -> help None
  | "suite" -> run_test_suite 2
  | "internal" -> run_internal_tests 2
  | _ -> help (Some ("unknown command " ^ Sys.argv.(1)))
