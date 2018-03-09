let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

open Printf

let print_json_value json = 
  let rec fmt value =
    match value with
    | `Assoc o -> printf "{"; print_json_assoc o; printf "}"
    | `List l -> printf "["; print_json_list l; printf "]"
    | `Null -> printf "Null "
    | `Bool b -> printf "%s " (if b then "true" else "false")
    | `Int i -> printf "%d " i
    | `Intlit s -> printf "%s " s
    | `Float f -> printf "%g" f
    | `Floatlit s -> printf "%s " s
    | `String s -> printf "%s " s
    | `Stringlit s -> printf "%s " s
    | `Tuple t -> printf "tuple "
    | `Variant t -> printf "variant "
  and print_json_assoc o = List.iter print_pair o
  and print_pair (k, v) = printf "%s : " k; fmt v; printf ","
  and print_json_list l = List.iter (fun v -> fmt v; printf ",") l
  in
  fmt json

module IO = struct
  type 'a t = 'a
  
  let return v = v
  let (>>=) a f = f a
end

let parsit contents =
  match Jsonxt.Basic.json_of_string contents with
  | Ok json -> print_json_value json; printf "\n"
  | Error s -> printf "%s\n" s

let () = 
  if Array.length Sys.argv < 2 then
    printf "expected filename\n"
  else
    let filename = Sys.argv.(1) in
    let contents = load_file filename in
      parsit contents
