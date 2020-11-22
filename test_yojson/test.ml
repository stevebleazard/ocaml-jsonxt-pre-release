open Yojson

let read contents = Raw.from_string contents

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (Bytes.to_string s)

let benchit () = 
  let contents = load_file "../test.json.10000" in
  (fun () -> read contents)
  
(*
let () =
  let contents = load_file "../test.json.10000" in
  let json = read contents in
  Raw.pretty_to_channel stdout json
*)

open Core
open Core_bench
let () = Command.run (Bench.make_command [
  let test = benchit () in
  Bench.Test.create ~name:"yojson" test
])
(*
*)
