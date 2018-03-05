open Yojson
open Core
open Core_bench.Std

let read () = Raw.from_file "../test.json.10000"

(*
let () =
  let json = read () in
  Raw.pretty_to_channel stdout json
*)

let () = Command.run (Bench.make_command [Bench.Test.create ~name:"yojson" read])
(*
*)
