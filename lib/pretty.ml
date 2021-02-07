module type Intf = sig
  val pp : ?std:bool -> Format.formatter -> 'a Json_internal.constrained -> unit
  val to_string : ?std:bool -> 'a Json_internal.constrained -> string
  val to_channel : ?std:bool -> out_channel -> 'a Json_internal.constrained -> unit
end

module Make(Compliance : Compliance.S) = struct
  let to_json_string s =
    let buf = Buffer.create 100 in
    let add_char = Buffer.add_char buf in
    let add_string = Buffer.add_string buf in
    let add_quote_string s = add_char '"'; Utils.escape ~add_char ~add_string s; add_char '"' in
    add_quote_string s;
    Buffer.contents buf

  let pp_list sep ppx out l =
    let pp_sep out () = Format.fprintf out "%s@ " sep in
    Format.pp_print_list ~pp_sep ppx out l

  let rec format std (out:Format.formatter) json : unit =
    match json with
      | `Null -> Format.pp_print_string out "null"
      | `Bool b -> Format.pp_print_bool out b
      | `Int i -> Format.pp_print_string out (string_of_int i)
      | `Float f ->
        let s = Compliance.number_to_string f in
        Format.pp_print_string out s
      | `String s -> Format.pp_print_string out (to_json_string s)
      | `Intlit s
      | `Floatlit s
      | `Stringlit s -> Format.pp_print_string out s
      | `List [] -> Format.pp_print_string out "[]"
      | `List l -> Format.fprintf out "[@;<1 0>@[<hov>%a@]@;<1 -2>]" (pp_list "," (format std)) l
      | `Assoc [] -> Format.pp_print_string out "{}"
      | `Assoc l ->
        Format.fprintf out "{@;<1 0>%a@;<1 -2>}" (pp_list "," (format_field std)) l
      | `Tuple l ->
        (*
        if std then format std out ((`List l):>'a Json_internal.constrained)
        else begin
        end
        *)
          if l = [] then Format.pp_print_string out "()"
          else Format.fprintf out "(@,%a@;<0 -2>)" (pp_list "," (format std)) l
      | `Variant (s, None) ->
        (*
        if std then format std out (`String s)
        else
        *)
        Format.fprintf out "<%s>" s
      | `Variant (s, Some json) ->
        (*
        if std then format std out ((`List [ `String s; json ]):>'a Json_internal.constrained)
        else begin
        end
        *)
          let s = to_json_string s in
          Format.fprintf out "<@[<hv2>%s: %a@]>" s (format std) json

  and format_field std out (name, json) =
    Format.fprintf out "@[<hv2>%s: %a@]" (to_json_string name) (format std) json

  let pp ?(std = false) out json =
    Format.fprintf out "@[<hv2>%a@]" (format std) json

  let to_string ?std json =
    Format.asprintf "%a" (pp ?std) json

  let to_channel ?std oc json =
    let fmt = Format.formatter_of_out_channel oc in
    Format.fprintf fmt "%a@?" (pp ?std) json

end
