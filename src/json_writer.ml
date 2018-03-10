open Printf

let to_string json = 
  let buf = Buffer.create 100 in
  let add_char = Buffer.add_char buf in
  let add_string = Buffer.add_string buf in
  let add_quote_string s = add_char '"'; add_string s; add_char '"' in (* CR escaping *)
  let add_int i = add_string (string_of_int i) in
  let add_float f = add_string (string_of_float f) in
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
  and pair (k, v) = add_string k; add_char ':'; fmt v
  and json_list l =
    let sep = ref "" in List.iter (fun v -> add_string !sep; sep := ","; fmt v ) l
  and variant (k, j) =
    add_quote_string k;
    match j with
    | Some j -> add_char ':'; fmt j
    | None -> ()
  in
  fmt json;
  Buffer.contents buf
