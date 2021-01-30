module Strict : sig
  type json = Json.Strict.json
  val member : string -> [> `Assoc of (string * json) list ] -> json
  val index : int -> [> `List of json list ] -> json
  val map :
    (json -> json) -> [> `List of json list ] -> [> `List of json list ]
  val to_assoc :
    [> `Assoc of (string * json) list ] -> (string * json) list
  val to_bool : [> `Bool of bool ] -> bool
  val to_float : [> `Float of float ] -> float
  val to_list : [> `List of json list ] -> json list
  val to_option : (([> `Null ] as 'a) -> json) -> 'a -> json option
  val to_bool_option : [> `Bool of bool | `Null ] -> bool option
  val to_float_option : [> `Float of float | `Null ] -> float option
end

module Basic : sig
  type json = Json.Basic.json
  val member : string -> [> `Assoc of (string * json) list ] -> json
  val index : int -> [> `List of json list ] -> json
  val map :
    (json -> json) -> [> `List of json list ] -> [> `List of json list ]
  val to_assoc :
    [> `Assoc of (string * json) list ] -> (string * json) list
  val to_bool : [> `Bool of bool ] -> bool
  val to_float : [> `Float of float ] -> float
  val to_list : [> `List of json list ] -> json list
  val to_option : (([> `Null ] as 'a) -> json) -> 'a -> json option
  val to_bool_option : [> `Bool of bool | `Null ] -> bool option
  val to_float_option : [> `Float of float | `Null ] -> float option
end
