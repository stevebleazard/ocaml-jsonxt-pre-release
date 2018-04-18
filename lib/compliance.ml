module type S = sig
  type json
  type json_stream

  val lex_number : Tokens.token -> Tokens.token
  val lex_integer : Tokens.token -> Tokens.token
  val lex_largeint : Tokens.token -> Tokens.token
  val lex_tuple : Tokens.token -> bool
  val lex_variant : Tokens.token -> bool

  val number_to_string : float -> string

  val number : [`Float of float | `Infinity | `Neginfinity | `Nan ] -> json
  val integer : int -> json
  val largeint : string -> json
  val null : json
  val string : string -> json
  val bool : bool -> json
  val assoc : (string * json) list -> json
  val list : json list -> json
  val tuple : json list -> json
  val variant : string -> json option -> json

  (* streaming functions *)

  module Stream : sig
    val number : [`Float of float | `Infinity | `Neginfinity | `Nan ] -> json_stream
    val integer : int -> json_stream
    val largeint : string -> json_stream
    val null : json_stream
    val string : string -> json_stream
    val bool : bool -> json_stream

    val array_start : unit -> json_stream
    val array_end : unit -> json_stream
    val object_start : unit -> json_stream
    val object_end : unit -> json_stream
    val tuple_start : unit -> json_stream
    val tuple_end : unit -> json_stream
    val variant_start : unit -> json_stream
    val variant_end : unit -> json_stream
    val name : string -> json_stream
  end
end

module Strict = struct
  type json = Json.Strict.json
  type json_stream = Json_stream.Strict.json

  open Tokens

  let lex_number = function
  | INFINITY -> COMPLIANCE_ERROR "inf not supported"
  | NEGINFINITY -> COMPLIANCE_ERROR "-inf not supported"
  | NAN -> COMPLIANCE_ERROR "nan not supported"
  | FLOAT _ as token -> token
  | token -> token

  let lex_integer token = token

  let lex_largeint = function
  | LARGEINT s -> FLOAT (float_of_string s)
  | token -> token

  let lex_variant _ = false
  let lex_tuple _ = false

  let number_to_string f =
    match classify_float f with
    | FP_normal | FP_subnormal | FP_zero -> Json_float.string_of_float_fast_int f
    | FP_infinite -> raise (Failure "infinity not supported")
    | FP_nan -> raise (Failure "nan not supported")

  let largeint s = `Float (float_of_string s)
  let integer i = `Float (float_of_int i)
  let null = `Null
  let string s = `String s
  let bool b = `Bool b
  let assoc a = `Assoc a
  let list l = `List l
  let tuple l = raise (Failure "tuples not supported in strict mode")
  let variant l = raise (Failure "variants not supported in strict mode")

  let number = function
  | `Float f ->     `Float f
  | `Infinity ->    raise (Failure "inf not supported in strict mode")
  | `Neginfinity -> raise (Failure "-inf not supported in strict mode")
  | `Nan ->         raise (Failure "nan not supported in strict mode")

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
    let tuple_start () = raise (Failure "tuples not supported in strict mode")
    let tuple_end () = raise (Failure "tuples not supported in strict mode")
    let variant_start () = raise (Failure "variants not supported in strict mode")
    let variant_end () = raise (Failure "variants not supported in strict mode")
    let name s = `Name s
  end
end

module Basic = struct
  type json = Json.Basic.json
  type json_stream = Json_stream.Basic.json

  open Tokens

  let lex_number = function
  | INFINITY -> COMPLIANCE_ERROR "inf not supported"
  | NEGINFINITY -> COMPLIANCE_ERROR "-inf not supported"
  | NAN -> COMPLIANCE_ERROR "nan not supported"
  | FLOAT _ as token -> token
  | token -> token

  let lex_integer token = token (* CR sbleazard: fix bounds *)

  let lex_largeint = function
  | LARGEINT s -> FLOAT (float_of_string s)
  | token -> token

  let lex_variant _ = false
  let lex_tuple _ = false

  let number_to_string f =
    match classify_float f with
    | FP_normal | FP_subnormal | FP_zero -> Json_float.string_of_float_fast_int f
    | FP_infinite -> raise (Failure "infinity not supported")
    | FP_nan -> raise (Failure "nan not supported")

  let largeint s = `Float (float_of_string s)
  let integer i = `Int i
  let null = `Null
  let string s = `String s
  let bool b = `Bool b
  let assoc a = `Assoc a
  let list l = `List l
  let tuple l = raise (Failure "tuples not supported in basic mode")
  let variant l = raise (Failure "variants not supported in basic mode")

  let number = function
  | `Float f ->     `Float f
  | `Infinity ->    raise (Failure "inf not supported in basic mode")
  | `Neginfinity -> raise (Failure "-inf not supported in basic mode")
  | `Nan ->         raise (Failure "nan not supported in basic mode")

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
    let tuple_start () = raise (Failure "tuples not supported in basic mode")
    let tuple_end () = raise (Failure "tuples not supported in basic mode")
    let variant_start () = raise (Failure "variants not supported in basic mode")
    let variant_end () = raise (Failure "variants not supported in basic mode")
    let name s = `Name s
  end
end

module Extended = struct
  type json = Json.json
  type json_stream = Json_stream.json

  open Tokens

  let lex_number token = token
  let lex_integer token = token

  let lex_largeint = function
  | LARGEINT s -> FLOAT (float_of_string s)
  | token -> token

  let lex_variant _ = true
  let lex_tuple _ = true

  let number_to_string f = Json_float.string_of_float_fast_int f

  let largeint s = `Float (float_of_string s)
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

module Yojson = struct
  module Basic = struct
    type json = Json.Basic.json
    type json_stream = Json_stream.Basic.json

    open Tokens

    let lex_number token = token
    let lex_integer token = token
    let lex_largeint _ = COMPLIANCE_ERROR "Integer out of bounds"

    let lex_variant _ = false
    let lex_tuple _ = false

    let number_to_string f =
      match classify_float f with
      | FP_normal | FP_subnormal | FP_zero ->
        Json_float.string_of_float_fast_int f
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
    let tuple l = raise (Failure "tuples not supported in yojson basic mode")
    let variant l = raise (Failure "variants not supported in yojson basic mode")

    let number = function
    | `Float f ->     `Float f
    | `Infinity ->    `Float (1.0 /. 0.0)
    | `Neginfinity -> `Float (-1.0 /. 0.0)
    | `Nan ->         `Float (0.0 /. 0.0)

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

  module Safe = struct
    type json = Json.Extended.json
    type json_stream = Json_stream.Extended.json

    open Tokens

    let lex_number token = token
    let lex_integer token = token
    let lex_largeint token = token

    let lex_variant _ = true
    let lex_tuple _ = true

    let number_to_string f =
      match classify_float f with
      | FP_normal | FP_subnormal | FP_zero ->
        Json_float.string_of_float_fast_int f
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
end
