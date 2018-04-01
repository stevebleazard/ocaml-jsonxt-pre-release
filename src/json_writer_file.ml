module type Intf = sig
  val json_to_file : string -> 'a Json_internal.constrained -> (unit, string) result
  val json_to_file_exn : string -> 'a Json_internal.constrained -> unit
  val json_to_channel :  out_channel -> 'a Json_internal.constrained -> (unit, string) result
  val json_to_channel_exn :  out_channel -> 'a Json_internal.constrained -> unit
  val json_to_file_hum : string -> 'a Json_internal.constrained -> (unit, string) result
  val json_to_file_hum_exn : string -> 'a Json_internal.constrained -> unit
  val json_to_channel_hum :  out_channel -> 'a Json_internal.constrained -> (unit, string) result
  val json_to_channel_hum_exn :  out_channel -> 'a Json_internal.constrained -> unit
  val to_file : string -> 'a Json_internal.constrained -> unit
  val to_file_hum : string -> 'a Json_internal.constrained -> unit
  val to_channel :  out_channel -> 'a Json_internal.constrained -> unit
  val to_channel_hum :  out_channel -> 'a Json_internal.constrained -> unit
end

module Make (Compliance : Compliance.S) : Intf = struct

  open Printf

  let nibble_to_hex i = char_of_int (if i > 9 then 65 + i - 10 else 48 + i)

  let add_hex_byte oc i =
    output_char oc (nibble_to_hex ((i lsr 4) land 0x0f));
    output_char oc (nibble_to_hex (i land 0x0f))

  let escape oc s =
    let add_char = output_char oc in
    let add_string = output_string oc in
    let l = String.length s in
    for i = 0 to l - 1 do
      match s.[i] with
      | '"'    -> add_string "\\\""
      | '\\'   -> add_string "\\\\"
      | '\b'   -> add_string "\\b"
      | '\012' -> add_string "\\f"
      | '\n'   -> add_string "\\n"
      | '\r'   -> add_string "\\r"
      | '\t'   -> add_string "\\t"
      | '\x00'..'\x1F'
      | '\x7F' as c ->
        add_string "\\u00";  add_hex_byte oc (int_of_char c)
      | _      -> add_char s.[i]
    done
   
  let json_to_channel' oc json = 
    let add_char = output_char oc in
    let add_string = output_string oc in
    let add_quote_string s = add_char '"'; escape oc s; add_char '"' in
    let add_int i = add_string (string_of_int i) in
    let add_float f = add_string (Compliance.number_to_string f) in
    let rec fmt value =
      match value with
      | `Assoc o -> add_char '{'; json_assoc o; add_char '}'
      | `List l -> add_char '['; json_list l; add_char ']'
      | `Null -> add_string "null"
      | `Bool b -> add_string (string_of_bool b)
      | `Int i -> add_int i
      | `Intlit s -> add_quote_string s
      | `Float f -> add_float f
      | `Floatlit s -> add_quote_string s
      | `String s -> add_quote_string s
      | `Stringlit s -> add_quote_string s
      | `Tuple t -> add_char '('; json_list t; add_char ')'
      | `Variant v -> add_char '<';  variant v; add_char '>'
    and json_assoc o =
      let sep = ref "" in List.iter (fun v -> add_string !sep; sep := ","; pair v ) o
    and pair (k, v) = add_quote_string k; add_char ':'; fmt v
    and json_list l =
      let sep = ref "" in List.iter (fun v -> add_string !sep; sep := ","; fmt v ) l
    and variant (k, j) =
      add_quote_string k;
      match j with
      | Some j -> add_char ':'; fmt j
      | None -> ()
    in
    fmt json

  let json_to_channel_hum' oc json = 
    let add_char = output_char oc in
    let add_string = output_string oc in
    let add_quote_string s = add_char '"'; escape oc s; add_char '"' in
    let add_int i = add_string (string_of_int i) in
    let add_float f = add_string (Compliance.number_to_string f) in
    let rec fmt ldr value =
      match value with
      | `Assoc o ->
        add_string "{\n"; json_assoc (ldr ^ "  ") o;
        add_char '\n'; add_string ldr; add_char '}'
      | `List l ->
        add_string "[\n"; json_list (ldr ^ "  ") l;
        add_char '\n'; add_string ldr; add_char ']'
      | `Null -> add_string "null"
      | `Bool b -> add_string (string_of_bool b)
      | `Int i -> add_int i
      | `Intlit s -> add_quote_string s
      | `Float f -> add_float f
      | `Floatlit s -> add_quote_string s
      | `String s -> add_quote_string s
      | `Stringlit s -> add_quote_string s
      | `Tuple t ->
        add_string "(\n"; json_list (ldr ^ "  ") t;
        add_char '\n'; add_string ldr; add_char ')'
      | `Variant v ->
        add_string "<";  variant (ldr ^ "  ") v;
        add_char '\n'; add_string ldr; add_char '>'
    and json_assoc ldr o =
      let sep = ref ldr in
      let newsep = ",\n" ^ ldr in
      List.iter (fun v -> add_string !sep; sep := newsep; pair ldr v ) o
    and pair ldr (k, v) = add_quote_string k; add_string ": "; fmt ldr v
    and json_list ldr l =
      let sep = ref ldr in
      let newsep = ",\n" ^ ldr in
      List.iter (fun v -> add_string !sep; sep := newsep; fmt ldr  v ) l
    and variant ldr (k, j) =
      add_quote_string k;
      match j with
      | Some j -> add_string ": "; fmt (ldr ^ "  ") j
      | None -> ()
    in
    fmt "" json

  let json_to_channel oc json =
    try Ok (json_to_channel' oc json) with
    | Failure err -> Error err

  let json_to_channel_hum oc json =
    try Ok (json_to_channel_hum' oc json) with
    | Failure err -> Error err

  let json_to_channel_exn = json_to_channel'
  let json_to_channel_hum_exn = json_to_channel_hum'

  let json_to_file file json =
    let oc = open_out file in
    let res = json_to_channel oc json in
    close_out oc;
    res

  let json_to_file_hum file json =
    let oc = open_out file in
    let res = json_to_channel_hum oc json in
    close_out oc;
    res

  let json_to_file_exn file json =
    let oc = open_out file in
    let _:unit = json_to_channel' oc json in
    close_out oc

  let json_to_file_hum_exn file json =
    let oc = open_out file in
    let _:unit = json_to_channel_hum' oc json in
    close_out oc

  let to_file = json_to_file_exn
  let to_file_hum = json_to_file_hum_exn
  let to_channel = json_to_channel_exn
  let to_channel_hum = json_to_channel_hum_exn
end
