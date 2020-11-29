%parameter<Compliance : Compliance.S>

%{
%}

%start <(Compliance.json option, string) result> lax
%%
lax:
  | EOF
    { Ok None }
  | v = value
    { Ok (Some v) }
  | error
    { Error "JSON syntax error" }
  | err = COMPLIANCE_ERROR
    { Error err }
  | err = LEX_ERROR
    { Error err }

value:
  | OS; obj = object_fields; OE
    { Compliance.assoc obj }
  | AS; l = list_values; AE
    { Compliance.list l }
  | s = STRING
    { Compliance.string s }
  | i = INT
    { Compliance.integer i }
  | s = LARGEINT
    (* LARGEINT is actually handled by the lexxer *)
    { Compliance.number (`Float (float_of_string s)) }
  | b = BOOL
    { Compliance.bool b }
  | f = FLOAT
    { Compliance.number (`Float f) }
  | INFINITY
    { Compliance.number `Infinity }
  | NEGINFINITY
    { Compliance.number `Neginfinity }
  | NAN
    { Compliance.number `Nan }
  | NULL
    { Compliance.null }


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

%%

