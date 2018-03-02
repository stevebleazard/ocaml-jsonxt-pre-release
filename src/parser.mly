%parameter<Compliance : sig
    type json

    val number : [`Float of float | `Infinity | `Neginfinity | `Nan ] -> json
    val integer : int -> json
    val null : json
    val string : string -> json
    val bool : bool -> json
    val assoc : (string * json) list -> json
    val list : json list -> json
  end>

(* This is necessary to allow the lexbuf to be accessed for error reporting
   as continuous position updates are disabled *)

%parameter<Error_handler : sig
    val get_lexbuf : unit -> Lexing.lexbuf option
  end>

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
    {
      let start = $startpos in
      let end_ = $endpos in
      let cnum =
        match Error_handler.get_lexbuf () with
        | None -> 0
        | Some lexbuf -> lexbuf.lex_last_pos - start.pos_bol
      in
      let err = Printf.sprintf "JSON syntax error at line %d char %d (%d, %d - %d, %d)" start.pos_lnum cnum start.pos_lnum start.pos_cnum end_.pos_lnum end_.pos_cnum
      in
        Error err
    }
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

