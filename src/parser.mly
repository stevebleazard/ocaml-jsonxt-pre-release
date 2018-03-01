%parameter<Compliance : sig
    type json

    val number : [`Float of float | `Infinity | `Neginfinity | `Nan ] -> json option
    val integer : int -> json option
    val null : json
    val string : string -> json
    val bool : bool -> json
    val assoc : (string * json) list -> json
    val list : json list -> json
  end>

%{
  exception Invalid_float
  exception Invalid_integer

  let validate_number num =
    match Compliance.number num with
    | None -> raise Invalid_float
    | Some json -> json
  
  let validate_integer num =
    match Compliance.integer num with
    | None -> raise Invalid_integer
    | Some json -> json

%}

%start <(Compliance.json option, string) result> lax
%%
lax:
  | EOF
    { Ok None }
  | v = value
    { Ok (Some v) }
  | error
    {
      let start = $startpos in
      let cnum = start.pos_cnum - start.pos_bol + 1 in
      let err = Printf.sprintf "JSON syntax error at line %d char %d" start.pos_lnum cnum in
        Error err
    }
  | err = COMPLIANCE_ERROR
    { Error err }

value:
  | OS; obj = object_fields; OE
    { Compliance.assoc obj }
  | AS; l = list_values; AE
    { Compliance.list l }
  | s = STRING
    { Compliance.string s }
  | i = INT
    { validate_integer i }
  | b = BOOL
    { Compliance.bool b }
  | f = FLOAT
    { validate_number (`Float f) }
  | INFINITY
    { validate_number `Infinity }
  | NEGINFINITY
    { validate_number `Neginfinity }
  | NAN
    { validate_number `Nan }
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

