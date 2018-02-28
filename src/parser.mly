%{
  let number = function
  | `Float f ->     `Float f
  | `Infinity ->    `Float (1.0 /. 0.0)
  | `Neginfinity -> `Float (-1.0 /. 0.0)
  | `Nan ->         `Float (0.0 /. 0.0)
%}

%start <Json.json option> lax
%%
lax:
  | EOF
    { None }
  | v = value
    { Some v }

value:
  | OS; obj = object_fields; OE
    { `Assoc obj }
  | AS; l = list_values; AE
    { `List l }
  | s = STRING
    { `String s }
  | i = INT
    { `Int i }
  | b = BOOL
    { `Bool b }
  | f = FLOAT
    { number (`Float f) }
  | INFINITY
    { number `Infinity }
  | NEGINFINITY
    { number `Neginfinity }
  | NAN
    { number `Nan }
  | NULL
    { `Null }

object_fields: obj = rev_object_fields { List.rev obj }

rev_object_fields:
  | (* empty *) { [] }
  | obj = rev_object_fields; COMMA; k = STRING; COLON; v = value
    { (k, v) :: obj }
  | k = STRING; COLON; v = value
    { [(k, v)] }

list_values: l = rev_list_values { List.rev l }

rev_list_values:
  | (* empty *) { [] }
  | l = rev_list_values; COMMA; v = value
    { v :: l }
  | v = value
    { [v] }

(*
%token <string> NAME
*) 
%%
