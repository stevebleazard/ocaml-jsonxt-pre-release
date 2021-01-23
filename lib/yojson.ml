exception Json_error of string

let json_error msg = raise (Json_error msg)

module Common (Compliance : Compliance.S) = struct
  module Internal = struct
    module Lexxer = Compliant_lexxer.Make(Compliance)
    module Parser = Parser.Make(Compliance)
    include Reader_string_file.Make (Lexxer) (Parser)

    include Writer_string.Make(Compliance)
    include Writer_file.Make(Compliance)
  end

  type json = Internal.json
  type t = json
  type json_line = [ `Json of t | `Exn of exn ]

  (* Helper functions *)
  let error_to_string (error_info:Error_info.t) fname lnum =
    let lnum = Option.value lnum ~default:1 in
    let info = { error_info with line = lnum + error_info.line - 1 } in
    let fname = match fname with
      | None -> "Line"
      | Some name -> "File" ^ name ^ ", line"
    in
    let loc = Printf.sprintf "%s %d chars %d-%d: " fname info.line info.start_char info.end_char in
    loc ^ info.msg

  let apply_and_handle_errors f a fname lnum =
    match f a with
    | Ok json -> json
    | Error error_info -> json_error (error_to_string error_info fname lnum)

  (* Readers *)
  let from_string ?buf:_ ?fname ?lnum s =
    apply_and_handle_errors Internal.json_of_string_error_info s fname lnum

  let from_channel ?buf:_ ?fname ?lnum in_channel =
    apply_and_handle_errors Internal.json_of_channel_error_info in_channel fname lnum

  let from_file ?buf:_ ?fname ?lnum filename =
    apply_and_handle_errors Internal.json_of_file_error filename fname lnum

  let stream_from_string ?buf:_ ?fname:_ ?lnum:_ s = Internal.stream_from_string s

  let stream_from_channel ?buf:_ ?(fin = fun () -> ()) ?fname:_ ?lnum:_ in_channel =
    Internal.stream_from_channel ~fin in_channel

  let stream_from_file ?buf:_ ?fname:_ ?lnum:_ filename =
    Internal.stream_from_file filename

  let linestream_from_channel ?buf:_ ?(fin = fun () -> ()) ?fname:_ ?lnum:_ ic =
    let f _i =
      try
        let line = input_line ic in Some (`Json (from_string line))
      with
        | End_of_file -> fin (); None
        | exn_ -> fin (); Some (`Exn exn_)
    in
    Stream.from f

  let linestream_from_file ?buf:_ ?fname:_ ?lnum:_ filename =
    let ic = open_in filename in
    linestream_from_channel ~fin:(fun () -> close_in ic) ic

  (* Writers *)
  let to_string ?buf:_ ?len:_ ?std:_ json = Internal.to_string json
  let to_channel ?buf:_ ?len:_ ?std:_ out_channel json = Internal.to_channel out_channel json
  let to_file ?len:_ ?std:_ filename json = Internal.to_file filename json
  let to_buffer ?std:_ buf json = Internal.to_buffer buf json
  let stream_to_string ?buf:_ ?len:_ ?std:_ stream = Internal.stream_to_string stream
  let stream_to_channel ?buf:_ ?len:_ ?std:_ out_channel stream = Internal.stream_to_channel out_channel stream
  let stream_to_file ?len:_ ?std:_ filename stream = Internal.stream_to_file filename stream
  let stream_to_buffer ?std:_ buf stream = Internal.stream_to_buffer buf stream
  let write_t buf json = to_buffer buf json

  (* Pretty printers *)
  let pretty_to_string ?std:_ json = Internal.to_string_hum json
  let pretty_to_channel ?std:_ oc json = Internal.to_channel_hum oc json

  (* Utilities *)
  let show json = Utilities.json_to_string_repr json
end

module Basic = struct
  module Compliance = struct
    type json = Json.Basic.json
    type json_stream = Json_stream.Basic.json

    open Tokens

    let lex_string s = Lexxer_utils.unescape_string s
    let lex_number token = token
    let lex_integer token = token
    let lex_largeint _ = COMPLIANCE_ERROR "Integer out of bounds"

    let lex_variant _ = false
    let lex_tuple _ = false

    let comment_check () = Ok ()

    let number_to_string f =
      match classify_float f with
      | FP_normal | FP_subnormal | FP_zero ->
        Json_float.string_of_float_json f
      | FP_infinite ->
        if f < 0. then "-Infinity" else "Infinity"
      | FP_nan ->
        "NaN"

    let largeint s = `Float (float_of_string s)
    let integer i = `Int i
    let null = `Null
    let string s = `String s
    let bool b = `Bool b
    let assoc a = `Assoc a
    let list l = `List l
    let tuple _l = raise (Failure "tuples not supported in yojson basic mode")
    let variant _l = raise (Failure "variants not supported in yojson basic mode")

    let number = function
    | `Float f ->     `Float f
    | `Infinity ->    `Float (1.0 /. 0.0)
    | `Neginfinity -> `Float (-1.0 /. 0.0)
    | `Nan ->         `Float (0.0 /. 0.0)
    | `Floatlit _ ->  raise (Failure "floatlit not supported in yojson basic mode")

    module Stream = struct
      let number = number
      let largeint = largeint
      let integer = integer
      let null = null
      let string = string
      let bool = bool

      let array_start () = `As
      let array_end () = `Ae
      let object_start () = `Os
      let object_end () = `Oe
      let tuple_start () = raise (Failure "tuples not supported in yojson basic mode")
      let tuple_end () = raise (Failure "tuples not supported in yojson basic mode")
      let variant_start () = raise (Failure "variants not supported in yojson basic mode")
      let variant_end () = raise (Failure "variants not supported in yojson basic mode")
      let name s = `Name s
    end
  end

  include Common(Compliance)

  let prettify ?std:_ instr = Internal.json_of_string_exn instr |> Internal.to_string_hum
  let compact ?std:_ instr = Internal.json_of_string_exn instr |> Internal.to_string
end

module Safe = struct
  module Compliance = struct
    type json =
      [
      | `Null
      | `Bool of bool
      | `Int of int
      | `Intlit of string
      | `Float of float
      | `String of string
      | `Assoc of (string * json) list
      | `List of json list
      | `Tuple of json list
      | `Variant of (string * json option)
      ]

    type json_stream = Json_stream.Extended.json  (* yojson interface does not support streaming *)

    let lex_string s = Lexxer_utils.unescape_string s
    let lex_number token = token
    let lex_integer token = token
    let lex_largeint token = token

    let lex_variant _ = true
    let lex_tuple _ = true

    let comment_check () = Ok ()

    let number_to_string f =
      match classify_float f with
      | FP_normal | FP_subnormal | FP_zero ->
        Json_float.string_of_float_json f
      | FP_infinite ->
        if f < 0. then "-Infinity" else "Infinity"
      | FP_nan ->
        "NaN"

    let largeint s = `Intlit s
    let integer i = `Int i
    let null = `Null
    let string s = `String s
    let bool b = `Bool b
    let assoc a = `Assoc a
    let list l = `List l
    let tuple l = `Tuple l
    let variant k v = `Variant (k, v)

    let number = function
    | `Float f ->     `Float f
    | `Infinity ->    `Float (1.0 /. 0.0)
    | `Neginfinity -> `Float (-1.0 /. 0.0)
    | `Nan ->         `Float (0.0 /. 0.0)
    | `Floatlit _ ->  raise (Failure "floatlit not supported in yojson safe mode")

    module Stream = struct
      let number = number
      let largeint = largeint
      let integer = integer
      let null = null
      let string = string
      let bool = bool

      let array_start () = `As
      let array_end () = `Ae
      let object_start () = `Os
      let object_end () = `Oe
      let tuple_start () = `Ts
      let tuple_end () = `Te
      let variant_start () = `Vs
      let variant_end () = `Ve
      let name s = `Name s
    end
  end

  include Common(Compliance)

  let prettify ?std:_ instr = Internal.json_of_string_exn instr |> Internal.to_string_hum
  let compact ?std:_ instr = Internal.json_of_string_exn instr |> Internal.to_string

  let to_basic json : Basic.json =
    let rec map node =
      match node with
      | `Null -> `Null
      | `Bool _ as v -> v
      | `Int _ as v -> v
      | `Intlit v -> `String v
      | `Float _ as v -> v
      | `String _ as v -> v
      | `List l -> `List (List.map map l)
      | `Assoc a -> `Assoc (List.map (fun (id, v) -> (id, map v)) a)
      | `Tuple tpl -> `List (List.map map tpl)
      | `Variant (name, jopt) ->
         match jopt with
         | None -> `String name
         | Some v -> `List [ `String name; (map v) ]
    in
    map json

end

module Raw = struct
  module Compliance = struct

  type json =
    [
    | `Null
    | `Bool of bool
    | `Intlit of string
    | `Floatlit of string
    | `Stringlit of string
    | `Assoc of (string * json) list
    | `List of json list
    | `Tuple of json list
    | `Variant of (string * json option)
    ]

    type json_stream = Json_stream.Extended.json (* yojson interface does not support streaming *)

    let lex_string s = "\"" ^ s ^ "\""
    let lex_number token = token
    let lex_integer token = token
    let lex_largeint token = token

    let lex_variant _ = true
    let lex_tuple _ = true

    let comment_check () = Ok ()

    let number_to_string f =
      match classify_float f with
      | FP_normal | FP_subnormal | FP_zero ->
        Json_float.string_of_float_json f
      | FP_infinite ->
        if f < 0. then "-Infinity" else "Infinity"
      | FP_nan ->
        "NaN"

    let largeint s = `Intlit s
    let integer i = `Intlit (string_of_int i)
    let null = `Null
    let string s = `Stringlit s
    let bool b = `Bool b
    let assoc a = `Assoc a
    let list l = `List l
    let tuple l = `Tuple l
    let variant k v = `Variant (k, v)

    let number = function
    | `Float f ->     `Floatlit (string_of_float f)
    | `Infinity ->    `Floatlit "Infinity"
    | `Neginfinity -> `Floatlit "-Infinity"
    | `Nan ->         `Floatlit "NaN"
    | `Floatlit f ->  `Floatlit f

    module Stream = struct
      let number = number
      let largeint = largeint
      let integer = integer
      let null = null
      let string = string
      let bool = bool

      let array_start () = `As
      let array_end () = `Ae
      let object_start () = `Os
      let object_end () = `Oe
      let tuple_start () = `Ts
      let tuple_end () = `Te
      let variant_start () = `Vs
      let variant_end () = `Ve
      let name s = `Name s
    end
  end

  include Common(Compliance)

  let prettify ?std:_ instr = Internal.json_of_string_exn instr |> Internal.to_string_hum
  let compact ?std:_ instr = Internal.json_of_string_exn instr |> Internal.to_string
end
