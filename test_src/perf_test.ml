let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

open Core
open Core_bench.Std

module Yj = struct
  open Yojson

  let read contents = Basic.from_string contents

  let benchit contents =
    let json = read contents in
    (fun () -> Basic.to_string json)

  let dumpit contents = read contents |> Basic.to_string |> Printf.printf "%s\n"
end

let testbuf buf = 
  let rec loop n =
    if n <= 0 then ()
    else begin Buffer.add_string buf "xxxxxxxxxxxxxxxx"; loop (n - 1) end
  in
    loop 100000;
    Buffer.reset buf

let benchbuf bsize =
  let buf = Buffer.create bsize in
  (fun () -> testbuf buf)

let benchit contents =
  let json = Jsonxt.Extended.of_string contents in
  (fun () -> Writer.to_string json)

let contents = load_file "test.json.10000"
let test = benchbuf 100
let testxt = benchit contents
let testyj = Yj.benchit contents

let () = Command.run (Bench.make_command [
    Bench.Test.create ~name:"buffer" test
  ; Bench.Test.create ~name:"jsonxt" testxt
  ; Bench.Test.create ~name:"yjson" testyj
  ])
