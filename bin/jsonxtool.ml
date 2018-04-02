open Printf

let die msg = 
  printf "ERROR: %s\n" msg;
  exit 255

let help_error err = 
  match err with
  | Some err -> printf "ERROR: %s\n\n" err
  | None -> ()

let help_msg msg err =
  help_error err;
  printf "%s" msg;
  exit 0

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  s

let command_dump idx sys_argv_len =
  let usage = "\
   jsonxtool dump [-h|-file file] [-compliance [strict|basic|extended|yjbasic|yjsafe]]

   Load the specified file or stdin of file is minus (-) and dump out in internal tree format.
   The compliance level can be selected, defaulting to extended.\n"
  in
  let help = ref false in
  let file = ref "" in
  let compliance = ref `Extended in
  let set_compliance = function
    | "strict" -> compliance := `Strict
    | "basic" -> compliance := `Basic
    | "extended" -> compliance := `Extended
    | "yjbasic" -> compliance := `Yojson_basic
    | "yjsafe" -> compliance := `Yojson_safe
    | level -> help_error (Some ("unknown compliance level: " ^ level))
  in
  let args = [
      ("-h", Arg.Set help, "show dump options")
    ; ("-file", Arg.Set_string file, "file to dump")
    ; ("-compliance",
       Arg.Symbol (["strict"; "basic"; "extended"; "yjbasic"; "yjsafe"], set_compliance),
       "compliance level")
    ]
  in
  let no_anon arg = help_error (Some ("unexptected anonymous argument: " ^ arg)) in
  let current = ref idx in
  if sys_argv_len < 3 then help_msg usage (Some "dump expected at least one option");
  Arg.parse_argv ~current Sys.argv args no_anon usage

let () =
  let usage = "jsonxtool [help|dump]\n" in
  let sys_argv_len = Array.length Sys.argv in
  if sys_argv_len < 2 then help_msg usage (Some "expected command");
  match Sys.argv.(1) with
  | "help" -> help_msg usage None
  | "dump" -> command_dump 2 sys_argv_len
  | _ -> help_msg usage (Some ("unknown command " ^ Sys.argv.(1)))
