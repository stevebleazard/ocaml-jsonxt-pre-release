(** json tree conversion functions

    Support converstion of a json tree to strict and basic types
*)

(** [to_basic json] converts [json] into the [Basic.json] type
    transforming variants that do not comply to that type.
    All json types can be converted *)
val to_basic : 'a Json_internal.constrained -> Basic.json

(** [to_basic json] converts [json] into the [Strict.json] type
    transforming variants that do not comply to that type.
    In particular ints are converted to floats. All json types can
    be converted *)
val to_strict : 'a Json_internal.constrained -> Strict.json
