# jsonxt - JSON parsers for files, strings and more

*jsonxt* provides a number of JSON parsers and writers for
RFC 8259 compliant JSON as well as non-standard extentions
introduced by Yojson.  Features include

* RFC 8259 compliant when in strict and basic mode
* Performance focused especially for files and strings
* Support for standard and extended JSON tree types:
  * Strict follows a strict interpretation of RFC 8259 with all
    numbers represented as floats.
  * Basic extends the strict type to include convenience types while maintaining
    RFC compliance.  This is compatible with yojson's Basic type
  * Extended adds additional non-standard types including tuples and variants
    and is not RFC compliant. This is compatible with yojson's Safe type
* A number of different parsers including
  * A standard JSON tree parser for various sources including string, file and channel
  * A Stream parser that returns a stream of raw JSON tokens.
  * A monad based parser compatible with async
* Writers including
  * File and string writers
  * A monad based writer that is compatible with async
  * A stream writer that converts a stream of JSON tokens
* Support for streaming JSON via Stream.t
* Standard interfaces including Yojson compatibility

# Quick Start
The following covers various use cases

## Convert a string and print the internal representation

```
let () =
  let json = Jsonxt.Basic.of_string "[1,2,3]" in
  print_endline (Jsonxt.Utilities.json_to_string_repr json);;
```

## Reading a file and printing to stdout

```
let () = 
  let json = Jsonxt.Basic.of_file "test.json" in
  Jsonxt.Basic.to_channel_hum stdout json;;
```

## Reading and writing a file using the monad functions

```
module IO = struct
  type 'a t = 'a
  let return v = v
  let (>>=) v f = f v
end

module JsonIO = Jsonxt.Basic_monad.Make(IO)

open IO
let _ =
  let ic = open_in "test.json" in
  let reader buf len = return (input ic buf 0 len) in
  let writer s = return (output_string stdout s) in
  JsonIO.read_json ~reader ()
  >>= function
    | Error err -> print_endline ("ERROR: " ^ err); return ()
    | Ok json -> JsonIO.write_json_hum ~writer json
;;
```

# Yojson compatibility and using ppx\_yojson\_conv
To use Jsonxt's Yojson compatibility module create a `yojson.ml` file in
the source directory of the project with the following contents:

```
include Jsonxt.Yojson
```

The following is an example using ppx\_yojson\_conv:

```
module Item = struct
  type t = {
    str : string
  ; cost : float
  } [@@deriving yojson]
end

module Stock = struct
  type t = {
    desc : string
  ; inventory : int
  ; backorder : int option
  ; items : Item.t list
  } [@@deriving yojson]
end

let () =
  let item1 = { Item.str = "Store Baked Beans"; cost = 1.22 } in
  let item2 = { Item.str = "Branded Baked Beans"; cost = 1.47 } in
  let stock = { Stock.desc = "Beans"; inventory = 2; backorder = Some 3; items = [item1; item2] } in
  let json = Stock.yojson_of_t stock in
  print_endline (Yojson.Safe.show json);
  print_endline (Yojson.Safe.pretty_to_string json);
```

See the examples/ppx\_yojson\_conv directory for a working example including the dune configuration
