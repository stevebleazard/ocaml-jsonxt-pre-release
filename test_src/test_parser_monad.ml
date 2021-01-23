module IO = struct
  type 'a t = 'a

  let return v = v
  let bind v f = f v
  let (>>=) v f = f v
end

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

open Printf

module JsonIO = Jsonxt.Basic_monad.Make(IO)
open IO

let parsit () =
  let reader buf len = return (input stdin buf 0 len) in
  let writer s = return (output_string stdout s) in
  JsonIO.read_json ~reader ()
  >>= function
    | Error err -> printf "ERROR %s\n" err; return ()
    | Ok json -> JsonIO.write_json_hum ~writer json


let _ = parsit ()
