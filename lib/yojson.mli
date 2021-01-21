module Basic : sig
  type json = Json.Basic.json
  type t = json
  type json_line = [ `Json of t | `Exn of exn ]

  (* Readers *)

  val from_string : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t
  val from_channel : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> in_channel -> t
  val from_file : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t
  val stream_from_string : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t Stream.t
  val stream_from_channel : ?buf:Buffer.t -> ?fin:(unit -> unit) -> ?fname:string -> ?lnum:int -> in_channel -> t Stream.t
  val stream_from_file : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t Stream.t

  val linestream_from_channel :
    ?buf:Buffer.t -> ?fin:(unit -> unit) -> ?fname:string -> ?lnum:int -> in_channel -> json_line Stream.t

  val linestream_from_file :
    ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> json_line Stream.t

  (* Writers *)

  val to_string : ?buf:Buffer.t -> ?len:int -> ?std:bool -> t -> string
  val to_channel : ?buf:Buffer.t -> ?len:int -> ?std:bool -> out_channel -> t -> unit
  val to_file : ?len:int -> ?std:bool -> string -> t -> unit
  val to_buffer : ?std:bool -> Buffer.t -> t -> unit
  val stream_to_string : ?buf:Buffer.t -> ?len:int -> ?std:bool -> t Stream.t -> string
  val stream_to_channel : ?buf:Buffer.t -> ?len:int -> ?std:bool -> out_channel -> t Stream.t -> unit
  val stream_to_file : ?len:int -> ?std:bool -> string -> t Stream.t -> unit
  val stream_to_buffer : ?std:bool -> Buffer.t -> t Stream.t -> unit
  val write_t : Buffer.t -> t -> unit
end

module Safe : sig
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

  type t = json
  type json_line = [ `Json of t | `Exn of exn ]

  (* Readers *)

  val from_string : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t
  val from_channel : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> in_channel -> t
  val from_file : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t
  val stream_from_string : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t Stream.t
  val stream_from_channel : ?buf:Buffer.t -> ?fin:(unit -> unit) -> ?fname:string -> ?lnum:int -> in_channel -> t Stream.t
  val stream_from_file : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t Stream.t

  val linestream_from_channel :
    ?buf:Buffer.t -> ?fin:(unit -> unit) -> ?fname:string -> ?lnum:int -> in_channel -> json_line Stream.t

  val linestream_from_file :
    ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> json_line Stream.t

  (* Writers *)

  val to_string : ?buf:Buffer.t -> ?len:int -> ?std:bool -> t -> string
  val to_channel : ?buf:Buffer.t -> ?len:int -> ?std:bool -> out_channel -> t -> unit
  val to_file : ?len:int -> ?std:bool -> string -> t -> unit
  val to_buffer : ?std:bool -> Buffer.t -> t -> unit
  val stream_to_string : ?buf:Buffer.t -> ?len:int -> ?std:bool -> t Stream.t -> string
  val stream_to_channel : ?buf:Buffer.t -> ?len:int -> ?std:bool -> out_channel -> t Stream.t -> unit
  val stream_to_file : ?len:int -> ?std:bool -> string -> t Stream.t -> unit
  val stream_to_buffer : ?std:bool -> Buffer.t -> t Stream.t -> unit
  val write_t : Buffer.t -> t -> unit

end

module Raw : sig
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

  type t = json
  type json_line = [ `Json of t | `Exn of exn ]

  (* Readers *)

  val from_string : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t
  val from_channel : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> in_channel -> t
  val from_file : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t
  val stream_from_string : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t Stream.t
  val stream_from_channel : ?buf:Buffer.t -> ?fin:(unit -> unit) -> ?fname:string -> ?lnum:int -> in_channel -> t Stream.t
  val stream_from_file : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t Stream.t

  val linestream_from_channel :
    ?buf:Buffer.t -> ?fin:(unit -> unit) -> ?fname:string -> ?lnum:int -> in_channel -> json_line Stream.t

  val linestream_from_file :
    ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> json_line Stream.t

  (* Writers *)

  val to_string : ?buf:Buffer.t -> ?len:int -> ?std:bool -> t -> string
  val to_channel : ?buf:Buffer.t -> ?len:int -> ?std:bool -> out_channel -> t -> unit
  val to_file : ?len:int -> ?std:bool -> string -> t -> unit
  val to_buffer : ?std:bool -> Buffer.t -> t -> unit
  val stream_to_string : ?buf:Buffer.t -> ?len:int -> ?std:bool -> t Stream.t -> string
  val stream_to_channel : ?buf:Buffer.t -> ?len:int -> ?std:bool -> out_channel -> t Stream.t -> unit
  val stream_to_file : ?len:int -> ?std:bool -> string -> t Stream.t -> unit
  val stream_to_buffer : ?std:bool -> Buffer.t -> t Stream.t -> unit
  val write_t : Buffer.t -> t -> unit

end
