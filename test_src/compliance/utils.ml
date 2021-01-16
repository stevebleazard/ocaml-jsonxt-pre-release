(* Determine the size of an integer, handles 31bit, 63bit and Jsoo using 32bit ints *)
let int_bits =
  let rec log2 n = if n <= 1 then 0 else 1 + log2(n asr 1) in
  let bits n = log2 n + 1 in
  match bits max_int with
  | 30|31|32 -> 32
  | _ -> 64

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
