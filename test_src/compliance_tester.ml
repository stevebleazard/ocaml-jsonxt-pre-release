module JsonSexp = struct
  open Core_kernel
  type json =
    [
    | `Null
    | `Bool of bool
    | `Int of int
    | `Intlit of string
    | `Float of float
    | `Floatlit of string
    | `String of string
    | `Stringlit of string
    | `Assoc of (string * json) list
    | `List of json list
    | `Tuple of json list
    | `Variant of (string * json option)
    ] [@@deriving sexp]
end

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
  Bytes.to_string s

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
    let of_error f a = match f a with Ok _ -> Ok () | Error _ -> Error () in
    let (of_string, of_file) = match level with
      | `Strict       ->
        let of_string = of_error Jsonxt.Strict.json_of_string in
        let of_file = of_error Jsonxt.Strict.json_of_file in
        (of_string, of_file)
      | `Basic        -> 
        let of_string = of_error Jsonxt.Basic.json_of_string in
        let of_file = of_error Jsonxt.Basic.json_of_file in
        (of_string, of_file)
      | `Extended     ->
        let of_string = of_error Jsonxt.Extended.json_of_string in
        let of_file = of_error Jsonxt.Extended.json_of_file in
        (of_string, of_file)
      | `Yojson_basic ->
        let of_string = of_error Jsonxt.Yojson.Basic.json_of_string in
        let of_file = of_error Jsonxt.Yojson.Basic.json_of_file in
        (of_string, of_file)
      | `Yojson_safe  ->
        let of_string = of_error Jsonxt.Yojson.Safe.json_of_string in
        let of_file = of_error Jsonxt.Yojson.Safe.json_of_file in
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
  

let help_suite err =
  help_error err;
  printf "\
   compliance_tester suite [help|run <file>|gen <file>]

   run the parser test suite, parsing and verifying each of the
   json strings defined in <file>. This is a tab seperated list
   of json and expected sexp in the format:
      json\\tsexp

   where
     json
       is the json to parse
     sexp
       is the expected sexp
       
   When in gen mode the sexp is ignored and a file
   suitable for using with run is output to stdout\n";
  exit 0

let execute_test_suite mode inc =
  let read_json_sexp () =
    let l = input_line inc in
    let p = String.split_on_char '\t' l in
    let (jsons, sexps) = match p with
      | jv::sv::[] -> (jv, sv)
      | jv::[] -> (jv, "")
      | _ -> die ("invalid test line: " ^ l)
    in
    (jsons, sexps)
  in
  let report result jsons sexps =
    let result = match result with
      | Ok json -> begin
        match Core_kernel.Sexp.compare (JsonSexp.sexp_of_json json) (Core_kernel.Sexp.of_string sexps) with
        | 0 -> "pass"
        | _ -> "check failed"
        end
      | Error _ -> "parse failed"
    in
    printf "%s...%s\n" result jsons
  in
  let output_sexp jsons json =
    printf "%s\t%s\n" jsons (JsonSexp.sexp_of_json json |> Core_kernel.Sexp.to_string)
  in
  let run_one jsons sexps =
    let json_result = Jsonxt.Extended.json_of_string jsons in
    match mode with
    | `Run -> report json_result jsons sexps
    | `Gen -> begin
        match json_result with
        | Ok json -> output_sexp jsons json
        | Error err -> sprintf "failed to parse \"%s\": %s" jsons err |> die
      end
  in
  let rec run () =
    let (jsons, sexps) = read_json_sexp () in
    run_one jsons sexps;
    run ()
  in
  try run () with
  | End_of_file -> ()


let run_test_suite idx =
  let args = Array.length Sys.argv - idx in
  if args <= 0 then help_suite (Some "expected run or help");
  match Sys.argv.(idx) with
  | "help" -> help_suite None
  | "run" | "gen" -> begin
    if args < 2 then help_suite (Some "expected filename of test schedule");
    let inf =
      try open_in Sys.argv.(idx + 1) with
      | _ -> help (Some ("failed to open " ^ Sys.argv.(idx + 1)))
    in
      let mode = match Sys.argv.(idx) with | "gen" -> `Gen | _ -> `Run in
      execute_test_suite mode inf;
      close_in inf
    end
  | cmd -> help_suite (Some ("unknown suite command " ^ cmd))


let () =
  if Array.length Sys.argv < 2 then help (Some "expected sub-test to run");
  match Sys.argv.(1) with
  | "help" -> help None
  | "suite" -> run_test_suite 2
  | "internal" -> run_internal_tests 2
  | _ -> help (Some ("unknown command " ^ Sys.argv.(1)))
