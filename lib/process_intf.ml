module type Shared = sig
  type json

  (** [member key json] searches the JSON object [json], which
      must be an [`Assoc] element, for [key] and returns the
      value or [`Null] of the [key] is missing *)
  val member : string -> [> `Assoc of (string * json) list ] -> json

  (** [index idx json] returns the [idx]-th JSON object in the [json] array,
      which must be an [`List] element. A negative [idx] value starts from
      the end with -1 being the last element.  A [Failure] exception is raise
      if the idx is out of bounds *)
  val index : int -> [> `List of json list ] -> json

  (** [map f json] applies the function [f] to each element of the JSON
      array [json], which must be an [`List] element, and returns a
      [`List] element *)
  val map :
    (json -> json) -> [> `List of json list ] -> [> `List of json list ]

  (** [to_assoc json] converts the JSON object [`Assoc a] to [a] *)
  val to_assoc :
    [> `Assoc of (string * json) list ] -> (string * json) list

  (** [to_bool json] converts [`Bool b] to [b] *)
  val to_bool : [> `Bool of bool ] -> bool

  (** [to_bool_option json] converts [`Bool b] to [Some b] and [`Null] to [None] *)
  val to_bool_option : [> `Bool of bool | `Null ] -> bool option

  (** [to_float json] converts [`Float f] to [f] *)
  val to_float : [> `Float of float ] -> float

  (** [to_float_option json] converts [`Float f] to [Some f] and [`Null] to [None] *)
  val to_float_option : [> `Float of float | `Null ] -> float option

  (** [to_option f json] returns [None] if [json] is [`Null] otherwise [Some (f json)].  *)
  val to_option : (([> `Null ] as 'a) -> json) -> 'a -> json option

  (** [to_list json] converts [`List l] to [l] *)
  val to_list : [> `List of json list ] -> json list

  (** [to_number json] converts [`Float f] to [f] *)
  val to_number : [> `Float of float ] -> float

  (** [to_number_option json] converts [`Float f] to [Some f] and [`Null] to [None] *)
  val to_number_option : [> `Float of float | `Null ] -> float option

  (** [convert_each f json] applies the function [f] to each element of the
      JSON array [json], which must be an [`List] element, and returns a
      list of the returned values. *)
  val convert_each : (json -> json) -> [> `List of json list ] -> json list

  (** [filter_map f l] applies [f] to each element of the list [l] and returns
      a new list with the values [v] for which [f] returned [Some v].  *)
  val filter_map : ('a -> 'a option) -> 'a list -> 'a list

  (** [rev_filter_map f acc l] applies [f] to each element of the list [l] and
      prepends the values for which [f] returned [Some v] to list [acc]. [acc]
      is retuned as the result.  This is a tail call optimised version of
      [filter_map] *)
  val rev_filter_map : ('a -> 'a option) -> 'a list -> 'a list -> 'a list

  val rev_flatten : 'a list -> [> `List of 'a list ] list -> 'a list
  val flatten : [> `List of 'a list ] list -> 'a list
  val filter_index : int -> [> `List of json list ] list -> json list
  val filter_list : [> `List of 'a ] list -> 'a list
  val filter_assoc : [> `Assoc of 'a ] list -> 'a list 
  val filter_bool : [> `Bool of bool ] list -> bool list
  val filter_float : [> `Float of float ] list -> float list
  val filter_string  : [> `String of string ] list -> string list
  val filter_member : string -> [> `Assoc of (string * json) list ] list -> json list
  val filter_number : [> `Float of float ] list -> float list
  val keys : [> `Assoc of (string * 'a) list ] -> string list
  val values : [> `Assoc of (string * 'a) list ] -> 'a list
  val combine : [> `Assoc of 'a list ] -> [> `Assoc of 'a list ] -> [> `Assoc of 'a list ]
end

module type Strict = sig
  type json

  (** [to_string json] converts [`String s] to [s] *)
  val to_string : [> `String of string ] -> string

  (** [to_string_option json] converts [`String s] to [Some s] and [`Null] to [None] *)
  val to_string_option : [> `String of string | `Null ] -> string option
end

module type Basic = sig
  type json

  (** [to_string json] converts [`String s] to [s] *)
  val to_string : [> `String of string ] -> string

  (** [to_string_option json] converts [`String s] to [Some s] and [`Null] to [None] *)
  val to_string_option : [> `String of string | `Null ] -> string option

  val to_number : [> `Int of int | `Float of float ] -> float
  val to_number_option : [> `Int of int | `Float of float | `Null ] -> float option
  val to_int : [> `Int of int ] -> int
  val to_int_option : [> `Int of int | `Null ] -> int option
  val filter_int : [> `Int of int ] list -> int list
  val filter_number : [> `Int of int | `Float of float ] list -> float list
end

module type Extended = sig
  type json

  val to_string :
    [> `String of string | `Intlit of string | `Floatlit of string | `Stringlit of string ] -> string
  val to_string_option :
    [> `String of string | `Intlit of string | `Floatlit of string | `Stringlit of string | `Null ] -> string option
  val to_number : [> `Int of int | `Float of float ] -> float
  val to_number_option : [> `Int of int | `Float of float | `Null ] -> float option
  val to_int : [> `Int of int ] -> int
  val to_int_option : [> `Int of int | `Null ] -> int option
  val filter_int : [> `Int of int ] list -> int list
  val filter_number : [> `Int of int | `Float of float ] list -> float list
end

module type Yojson_safe = sig
  type json

  val to_string : [> `String of string | `Intlit of string ] -> string
  val to_string_option : [> `String of string | `Intlit of string | `Null ] -> string option
  val to_number : [> `Int of int | `Float of float ] -> float
  val to_number_option : [> `Int of int | `Float of float | `Null ] -> float option
  val to_int : [> `Int of int ] -> int
  val to_int_option : [> `Int of int | `Null ] -> int option
  val filter_int : [> `Int of int ] list -> int list
  val filter_number : [> `Int of int | `Float of float ] list -> float list
end
