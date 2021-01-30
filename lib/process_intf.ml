module type Shared = sig
  type json

  val member : string -> [> `Assoc of (string * json) list ] -> json
  val index : int -> [> `List of json list ] -> json
  val map :
    (json -> json) -> [> `List of json list ] -> [> `List of json list ]
  val to_assoc :
    [> `Assoc of (string * json) list ] -> (string * json) list
  val to_bool : [> `Bool of bool ] -> bool
  val to_float : [> `Float of float ] -> float
  val to_option : (([> `Null ] as 'a) -> json) -> 'a -> json option
  val to_list : [> `List of json list ] -> json list
  val to_bool_option : [> `Bool of bool | `Null ] -> bool option
  val to_float_option : [> `Float of float | `Null ] -> float option
  val to_number : [> `Float of float ] -> float
  val to_number_option : [> `Float of float | `Null ] -> float option
  val convert_each : (json -> json) -> [> `List of json list ] -> json list
  val rev_filter_map : ('a -> 'a option) -> 'a list -> 'a list -> 'a list
  val filter_map : ('a -> 'a option) -> 'a list -> 'a list
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

  val to_string : [> `String of string ] -> string
  val to_string_option : [> `String of string | `Null ] -> string option
end

module type Basic = sig
  type json

  val to_string : [> `String of string ] -> string
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
