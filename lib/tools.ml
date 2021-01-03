let json_tree_to_string json = 
  let buf = Buffer.create 100 in
  let add_char = Buffer.add_char buf in
  let add_string = Buffer.add_string buf in
  let add_quote_string s = add_char '"'; Utils.escape ~add_char ~add_string s; add_char '"' in
  let add_int i = add_string (string_of_int i) in
  let add_float f = add_string (Json_float.string_of_float_json f) in
  let rec fmt ldr value =
    match value with
    | `Assoc o ->
      add_string "`Assoc [\n";
      begin match o with | [] -> () | _ -> json_assoc (ldr ^ "  ") o; add_char '\n' end;
      add_string ldr; add_char ']'
    | `List l ->
      add_string "`List [\n";
      begin match l with | [] -> () | _ -> json_list (ldr ^ "  ") l; add_char '\n' end;
      add_string ldr; add_char ']'
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

let json_to_string json = 
  let buf = Buffer.create 100 in
  let add_char = Buffer.add_char buf in
  let add_string = Buffer.add_string buf in
  let add_quote_string s = add_char '"'; Utils.escape ~add_char ~add_string s; add_char '"' in
  let add_int i = add_string (string_of_int i) in
  let add_float f = add_string (Json_float.string_of_float_json f) in
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
    let sep = ref "" in
    let newsep = "," in
    List.iter (fun v -> add_string !sep; sep := newsep; pair v) o
  and pair (k, v) = add_quote_string k; add_string ":"; fmt v
  and json_list l =
    let sep = ref "" in
    let newsep = "," in
    List.iter (fun v -> add_string !sep; sep := newsep; fmt v ) l
  and variant (k, j) =
    add_quote_string k;
    match j with
    | Some j -> add_string ": "; fmt j
    | None -> ()
  in
  fmt json;
  Buffer.contents buf

