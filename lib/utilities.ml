let to_basic json : Basic.json =
  let rec map node =
    match node with
    | `Null -> `Null
    | `Bool _ as v -> v
    | `Int _ as v -> v
    | `Intlit v -> `Int (int_of_string v)
    | `Float _ as v -> v
    | `Floatlit v -> `Float (float_of_string v)
    | `String _ as v -> v
    | `Stringlit s -> begin
      match String.length s with
      | 0 | 1 -> `String s         (* malformed, should have double-quotes at start and end *)
      | _ -> `String (String.sub s 1 (String.length s - 2))
      end
    | `List l -> `List (List.map map l)
    | `Assoc a -> `Assoc (List.map (fun (id, v) -> (id, map v)) a)
    | `Tuple tpl -> `List (List.map map tpl)
    | `Variant (name, jopt) ->
       match jopt with 
       | None -> `String name
       | Some v -> `List [ `String name; (map v) ]
  in
  map json

let to_strict json : Strict.json =
  let rec map node =
    match node with
    | `Null -> `Null
    | `Bool _ as v -> v
    | `Int v -> `Float (float_of_int v)
    | `Intlit v -> `Float (float_of_string v)
    | `Float _ as v -> v
    | `Floatlit v -> `Float (float_of_string v)
    | `String _ as v -> v
    | `Stringlit s -> begin
      match String.length s with
      | 0 | 1 -> `String s         (* malformed, should have double-quotes at start and end *)
      | _ -> `String (String.sub s 1 (String.length s - 2))
      end
    | `List l -> `List (List.map map l)
    | `Assoc a -> `Assoc (List.map (fun (id, v) -> (id, map v)) a)
    | `Tuple tpl -> `List (List.map map tpl)
    | `Variant (name, jopt) ->
       match jopt with 
       | None -> `String name
       | Some v -> `List [ `String name; (map v) ]
  in
  map json

