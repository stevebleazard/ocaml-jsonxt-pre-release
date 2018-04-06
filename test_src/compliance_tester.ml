open Printf

let die msg = 
  printf "ERROR: %s\n" msg;
  exit 255

let help_error err = 
  match err with
  | Some err -> printf "ERROR: %s\n\n" err
  | None -> ()

let help err =
  help_error err;
  printf "compliance_tester [help|suite|internal]\n";
  exit 0

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  s

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
       the file containing the test, it is assumed to be in the current directory, 
       .json is automatically appended to the filename.
       
   Note that any number of spaces can seperate the fields.\n";
  exit 0

let execute_internal_tests inc =
  let read_params () =
    let l = input_line inc in
    let p = String.split_on_char ' ' l |> List.filter (fun v -> not (String.equal "" v)) in
    let (level, passfail, filename) = match p with
      | lv::pf::fn::[] -> (lv, pf, fn)
      | _ -> die ("invalid test line: " ^ l)
    in
    let level = match level with
      | "strict"   -> `Strict
      | "basic"    -> `Basic
      | "extended" -> `Extended
      | "yjsafe"   -> `Yojson_safe
      | "yjbasic"  -> `Yojson_basic
      | _ -> die ("invalid test line, first column invalid level: " ^ l)
    in
    let passfail = match passfail with
      | "pass" -> `Pass
      | "fail" -> `Fail
      | _ -> die ("invalid test line, second column must be pass or fail: " ^ l)
    in
    (level, passfail, filename)
  in
  let report typ passfail result =
    let result = match result with
      | Ok _ -> if passfail = `Pass then "pass" else "fail"
      | Error _ -> if passfail = `Fail then "pass" else "fail"
    in
    printf "%s(%s)" result typ
  in
  let run_one level passfail filename =
    let txt = try load_file (filename ^ ".json") with Sys_error err -> die err in
    let to_error f a = match f a with Ok _ -> Ok () | Error _ -> Error () in
    let (of_string, of_file) = match level with
      | `Strict       ->
        let of_string = to_error Jsonxt.Strict.json_of_string in
        let of_file = to_error Jsonxt.Strict.json_of_file in
        (of_string, of_file)
      | `Basic        -> 
        let of_string = to_error Jsonxt.Basic.json_of_string in
        let of_file = to_error Jsonxt.Basic.json_of_file in
        (of_string, of_file)
      | `Extended     ->
        let of_string = to_error Jsonxt.Extended.json_of_string in
        let of_file = to_error Jsonxt.Extended.json_of_file in
        (of_string, of_file)
      | `Yojson_basic ->
        let of_string = to_error Jsonxt.Yojson.Basic.json_of_string in
        let of_file = to_error Jsonxt.Yojson.Basic.json_of_file in
        (of_string, of_file)
      | `Yojson_safe  ->
        let of_string = to_error Jsonxt.Yojson.Safe.json_of_string in
        let of_file = to_error Jsonxt.Yojson.Safe.json_of_file in
        (of_string, of_file)
    in
    let str_res = of_string txt in
    let file_res = of_file (filename ^ ".json") in
    report "S" passfail str_res;
    printf " ";
    report "F" passfail file_res;
    printf "...%s\n" filename
  in
  let rec run () = 
    let (level, passfail, filename) = read_params () in
    run_one level passfail filename;
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
