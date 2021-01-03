(** json tree utility functions

    Support converstion of the json tree into strings
*)

(** [json_to_string_repr json] converts [json] into a string representation
    of the tree. This is not JSON but a textual represenation of the json
    tree. eg
      `Assoc [
        "Boo1": `List []
      ]
*)
val json_to_string_repr : 'a Json_internal.constrained -> string

(** [json_to_string json] converts the json tree to standard JSON
    in compact format. The function does not apply any type
    constraints to the json tree
*)
val json_to_string : 'a Json_internal.constrained -> string
