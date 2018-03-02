module type S = sig
  type json

  val number : [`Float of float | `Infinity | `Neginfinity | `Nan ] -> json
  val integer : int -> json
  val null : json
  val string : string -> json
  val bool : bool -> json
  val assoc : (string * json) list -> json
  val list : json list -> json
end
