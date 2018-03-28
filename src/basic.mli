type json = Json.Basic.json
type t = json

val json_of_string : ?strict:bool -> string -> (json, string) result
val json_of_string_exn : ?strict:bool -> string -> json
val json_of_file : ?strict:bool -> string -> (json, string) result
val json_of_file_exn : ?strict:bool -> string -> json
val json_of_string : ?strict:bool -> string -> (json, string) result
val of_string : string -> json

val json_to_string : json -> string
val to_string : json -> string

val json_to_file : string -> json -> unit
val json_to_channel : out_channel -> json -> unit
