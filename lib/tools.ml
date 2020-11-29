let nibble_to_hex i = char_of_int (if i > 9 then 65 + i - 10 else 48 + i)

let add_hex_byte buf i =
  Buffer.add_char buf (nibble_to_hex ((i lsr 4) land 0x0f));
  Buffer.add_char buf (nibble_to_hex (i land 0x0f))

let escape buf s =
  let add_char = Buffer.add_char buf in
  let add_string = Buffer.add_string buf in
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
      add_string "\\u00";  add_hex_byte buf (int_of_char c)
    | _      -> add_char s.[i]
  done
 
let dump json = 
  let buf = Buffer.create 100 in
  let add_char = Buffer.add_char buf in
  let add_string = Buffer.add_string buf in
  let add_quote_string s = add_char '"'; escape buf s; add_char '"' in
  let add_int i = add_string (string_of_int i) in
  let add_float f = add_string (Json_float.string_of_float_json f) in
  let rec fmt ldr value =
    match value with
    | `Assoc o ->
      add_string "`Assoc [\n"; json_assoc (ldr ^ "  ") o;
      add_char '\n'; add_string ldr; add_char ']'
    | `List l ->
      add_string "`List [\n"; json_list (ldr ^ "  ") l;
      add_char '\n'; add_string ldr; add_char ']'
    | `Null -> add_string "`Null"
    | `Bool b -> add_string "`Bool "; add_string (string_of_bool b)
    | `Int i -> add_string "`Int "; add_int i
    | `Intlit s -> add_string "`Intlit "; add_quote_string s
    | `Float f -> add_string "`Float "; add_float f
    | `Floatlit s -> add_string "`Floatlit "; add_quote_string s
    | `String s -> add_string "`String "; add_quote_string s
    | `Stringlit s -> add_string "`Stringlit "; add_quote_string s
    | `Tuple t ->
      add_string "`Tuple (\n"; json_list (ldr ^ "  ") t;
      add_char '\n'; add_string ldr; add_char ')'
    | `Variant v ->
      add_string "`Variant <";  variant (ldr ^ "  ") v;
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
  fmt "" json;
  Buffer.contents buf

