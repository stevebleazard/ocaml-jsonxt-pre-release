module JsonSexp = struct
  open Core_kernel
  type json = [
       `Null | `Bool of bool | `Int of int | `Intlit of string | `Float of float
    | `Floatlit of string | `String of string | `Stringlit of string
    | `Assoc of (string * json) list | `List of json list | `Tuple of json list
    | `Variant of (string * json option) ] [@@deriving sexp]
end

module JsonStreamSexp = struct
  open Core_kernel
  type json_stream = [
      `Null | `Bool of bool | `Int of int | `Intlit of string | `Float of float
    | `Floatlit of string | `String of string | `Stringlit of string
    | `As | `Ae | `Os | `Oe | `Ts | `Te | `Vs | `Ve | `Name of string
    | `Infinity | `Neg_infinity | `Nan ] list [@@deriving sexp]
end

let read_json_sexp inc =
  let l = input_line inc in
  let p = String.split_on_char '\t' l in
  let (jsons, sexps, sexps_json_stream) = match p with
    | jv::sv::ssv::[] -> (jv, sv, ssv)
    | jv::[] -> (jv, "", "")
    | _ -> Utils.die ("invalid test line: " ^ l)
  in
  (jsons, sexps, sexps_json_stream)

let output_validation_config jsons json json_stream =
  Printf.printf "%s\t%s\t%s\n"
    jsons
    (JsonSexp.sexp_of_json json |> Core_kernel.Sexp.to_string)
    (JsonStreamSexp.sexp_of_json_stream json_stream |> Core_kernel.Sexp.to_string)

let get_json_stream jsons =
  let stream = Jsonxt.Extended_stream.json_stream_of_string jsons in
  let rec loop res =
    match Jsonxt.Extended_stream.decode_stream stream with
    | Error err -> Error err
    | Ok None -> Ok (List.rev res)
    | Ok Some tok -> loop (tok::res)
  in
  loop []

let parse_json jsons =
  let json_result = Jsonxt.Extended.json_of_string jsons in
  let json_stream_result = get_json_stream jsons in
  let json = 
    match json_result with
    | Ok json -> json
    | Error err -> Printf.sprintf "failed to parse \"%s\": %s" jsons err |> Utils.die
  in
  let json_stream = 
    match json_stream_result with
    | Ok json_stream -> json_stream
    | Error err -> Printf.sprintf "failed to parse stream \"%s\": %s" jsons err |> Utils.die
  in
  (json, json_stream)

let gen_config filename _alco_opts =
  let inc = try open_in filename with | Sys_error err -> Utils.die err in
  let rec loop () =
    match read_json_sexp inc with
    | jsons, _sexps, _sexps_json_stream ->
      let json, json_stream = parse_json jsons in
      output_validation_config jsons json json_stream;
      loop ()
    | exception End_of_file -> ()
  in loop ()

let get_json_stream jsons =
  let stream = Jsonxt.Extended_stream.json_stream_of_string jsons in
  let rec loop res =
    match Jsonxt.Extended_stream.decode_stream stream with
    | Error err -> Error err
    | Ok None -> Ok (List.rev res)
    | Ok Some tok -> loop (tok::res)
  in
  loop []

let json_stream_to_string stream =
  let buf = Buffer.create 100 in
  let add_char c = Buffer.add_char buf c in
  let add_string s = Buffer.add_string buf s in
  let encoder = Jsonxt.Extended_stream.create_encoder ~add_char ~add_string in
  let encode () =
    List.iter (fun v -> Jsonxt.Extended_stream.encode_stream_exn encoder v) stream;
    Buffer.contents buf
  in
  try Ok (encode ()) with
  | Failure err -> Error err

let sexp =
  let pp ppf v = Fmt.pf ppf "%s" (Core_kernel.Sexp.to_string v) in
  let sexp_eq a b = match Core_kernel.Sexp.compare a b with | 0 -> true | _ -> false in
  Alcotest.testable pp sexp_eq

let string_parse_test jsons sexps () =
  let jsonsexp = 
    match Jsonxt.Extended.json_of_string jsons with
    | Ok json -> JsonSexp.sexp_of_json json
    | Error err -> Core_kernel.Sexp.Atom (Printf.sprintf "Failed to parse '%s': %s" jsons err)
  in
  let sexpv = Core_kernel.Sexp.of_string sexps in
  Alcotest.(check sexp) jsons sexpv jsonsexp 

let string_write_test jsons sexps () =
  let jsonsexp = 
    match Jsonxt.Extended.json_of_string jsons with
    | Ok json -> begin
      match Jsonxt.Extended.json_to_string json with
      | Ok str -> begin
          match Jsonxt.Extended.json_of_string str with
          | Ok json -> JsonSexp.sexp_of_json json
          | Error err -> Core_kernel.Sexp.Atom (Printf.sprintf "Failed to re-parse written json '%s': %s" str err)
        end
      | Error err -> Core_kernel.Sexp.Atom (Printf.sprintf "Failed to write parsed json '%s': %s" jsons err)
      end
    | Error err -> Core_kernel.Sexp.Atom (Printf.sprintf "Failed to parse '%s': %s" jsons err)
  in
  let sexpv = Core_kernel.Sexp.of_string sexps in
  Alcotest.(check sexp) jsons sexpv jsonsexp 

let stream_parse_test jsons sexp_streams () =
  let json_stream_sexp = 
    match get_json_stream jsons with
    | Ok json_stream -> JsonStreamSexp.sexp_of_json_stream json_stream
    | Error err -> Core_kernel.Sexp.Atom (Printf.sprintf "Failed to parse '%s': %s" jsons err)
  in
  let sexp_stream = Core_kernel.Sexp.of_string sexp_streams in
  Alcotest.(check sexp) jsons sexp_stream json_stream_sexp 
  
let gen_tests filename =
  let inc = try open_in filename with | Sys_error err -> Utils.die err in
  let rec loop std stream stdwrite =
    match read_json_sexp inc with
    | jsons, sexps, sexps_json_stream ->
      let msg = jsons in
      loop ((Alcotest.test_case msg `Quick (string_parse_test jsons sexps))::std)
           ((Alcotest.test_case msg `Quick (stream_parse_test jsons sexps_json_stream))::stream)
           ((Alcotest.test_case msg `Quick (string_write_test jsons sexps))::stdwrite)
    | exception End_of_file -> (std, stream, stdwrite)
  in
  let std_t, stream_t, strwrite_t = loop [] [] [] in
  [
    "standard", (List.rev std_t);
    "standard-write", (List.rev strwrite_t);
    "stream", (List.rev stream_t)
  ]

let run_tests filename alco_opts =
  let argv = Array.of_list ("compliance"::alco_opts) in
  Alcotest.run ~argv "Validation" (gen_tests filename)
