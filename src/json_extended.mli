type json = Json.Extended.json
type t = json

val json_of_string : ?strict:bool -> string -> (json, string) result
val json_of_string_exn : ?strict:bool -> string -> json
val json_of_file : ?strict:bool -> string -> (json, string) result
val json_of_file_exn : ?strict:bool -> string -> json
val of_string : string -> json
