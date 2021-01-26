exception Type_error of string * Json.json
exception Undefined of string * Json.json

let type_to_string = function
  | `Assoc _ -> "`Assoc"
  | `Bool _ -> "`Bool"
  | `Float _ -> "`Float"
  | `Int _ -> "`Int"
  | `List _ -> "`Array"
  | `Null -> "`Null"
  | `String _ -> "`String"
  | `Intlit _ -> "`Intlit"
  | `Tuple _ -> "`Tuple"
  | `Variant _ -> "`Variant"
  | `Floatlit _ -> "`Floatlit"
  | `Stringlit _ -> "`Stringlit"

let error msg json = raise (Type_error (msg ^ type_to_string json, json))
let assoc name obj = try List.assoc name obj with Not_found -> `Null

let member name = function
  | `Assoc obj -> assoc name obj
  | json -> error ("Expected `Assoc to find name '" ^ name ^ "' in, got ") json

let index i = function
  | `List l as json ->
      let len = List.length l in
      let i' = if i < 0 then len + i else i in
      if i' < 0 || i' >= len then raise (Undefined (string_of_int i ^ " out of bounds", json))
      else List.nth l i'
  | json -> error "Can't index none `List type " json

let map f = function
  | `List l -> `List (List.map f l)
  | json -> error "Can't map over none `List type " json

let to_assoc = function | `Assoc obj -> obj | json -> error "Expected `Assoc, got " json
let to_bool = function | `Bool b -> b | json -> error "Expected `Bool, got " json
let to_float = function | `Float f -> f | json -> error "Expected `Float, got " json
let to_int = function | `Int i -> i | json -> error "Expected `Int, got " json
let to_list = function | `List l -> l | json -> error "Expected `List, got " json
let to_option f = function | `Null -> None | v -> Some (f v)

let to_bool_option = function
  | `Bool b -> Some b
  | `Null -> None
  | json -> error "Expected `Bool or `Null, got " json

let to_number = function
  | `Int i -> float i
  | `Float f -> f
  | json -> error "Expected `Int or `Float, got " json

let to_number_option = function
  | `Int i -> Some (float i)
  | `Float f -> Some f
  | `Null -> None
  | json -> error "Expected `Int, `Float or `Null, got " json

let to_float_option = function
  | `Float f -> Some f
  | `Null -> None
  | json -> error "Expected `Float or `Null, got " json

let to_int_option = function
  | `Int i -> Some i
  | `Null -> None
  | json -> error "Expected `Int or `Null, got " json

let to_string = function
  | `String s -> s
  | `Intlit s -> s
  | `Floatlit s -> s
  | `Stringlit s ->
    if String.length s > 1 && s.[0] = '"' && s.[String.length s - 1] = '"' then
      String.sub s 1 (String.length s - 1)
    else
      s
  | json -> error "Expected `String, got " json

let to_string_option = function
  | `String s -> Some s
  | `Intlit s -> Some s
  | `Floatlit s -> Some s
  | `Stringlit s ->
    if String.length s > 1 && s.[0] = '"' && s.[String.length s - 1] = '"' then
      Some (String.sub s 1 (String.length s - 1))
    else
      Some s
  | `Null -> None
  | json -> error "Expected `String or `Null, got " json

let convert_each f = function
  | `List l -> List.map f l
  | json -> error "Expected `List, got " json

let rec rev_filter_map f acc l =
  match l with
  | [] -> acc
  | hd::tl ->
    match f hd with
    | None -> rev_filter_map f acc tl
    | Some v -> rev_filter_map f (v::acc) tl

let filter_map f l = List.rev (rev_filter_map f [] l)

let rec rev_flatten acc l =
  match l with
  | [] -> acc
  | hd::tl ->
    match hd with
    | `List l2 -> rev_flatten (List.rev_append l2 acc) tl
    | _ -> rev_flatten acc tl

let flatten l = List.rev (rev_flatten [] l)

let filter_index i l =
  filter_map (function | `List l -> (try Some (List.nth l i) with _ -> None) | _ -> None) l

let filter_list l = filter_map (function `List l -> Some l | _ -> None) l
let filter_assoc l = filter_map (function `Assoc l -> Some l | _ -> None) l
let filter_bool l = filter_map (function `Bool b -> Some b | _ -> None) l
let filter_int l = filter_map (function `Int i -> Some i | _ -> None) l
let filter_float l = filter_map (function `Float f -> Some f | _ -> None) l
let filter_string l = filter_map (function `String s -> Some s | _ -> None) l

let filter_member k l =
  filter_map (function `Assoc l -> (try Some (List.assoc k l) with _ -> None) | _ -> None) l

let filter_number l =
  filter_map (
    function
    | `Int i -> Some (float i)
    | `Float f -> Some f
    | _ -> None
  ) l

let keys o = to_assoc o |> List.map (fun (key, _) -> key)
let values o = to_assoc o |> List.map (fun (_, value) -> value)

let combine (first : Json.json) (second : Json.json) =
  match (first, second) with
  | (`Assoc a, `Assoc b) -> (`Assoc (a @ b) :  Json.json)
  | (_, _) -> raise (Invalid_argument "Expected two objects")
