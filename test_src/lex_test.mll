{
type token = EOL | INT of int | PLUS | EOF

open Lexing

module Make (M : sig
               type 'a t
               val return: 'a -> 'a t
               val bind: 'a t -> ('a -> 'b t) -> 'b t
               val fail : string -> 'a t

               val read : Bytes.t -> int -> int t
             end)
= struct

  let on_refill lexbuf =
    let buf = Bytes.create 512 in
    M.bind (M.read buf 512) (fun len ->
      Lexutils.fill_lexbuf buf len lexbuf;
      M.return ())

  let refill_handler k lexbuf =
      M.bind (on_refill lexbuf) (fun () -> k lexbuf)

}


refill {refill_handler}

rule token = parse 
| [' ' '\t']
    { token lexbuf  }
| '\n'
    { M.return EOL }
| ['0'-'9']+ as i
    { M.return (INT (int_of_string i)) }
| '+'
    { M.return PLUS }
| eof
    { M.return EOF }
| _
    { M.fail "unexpected character" }

{
end

let create_lexxer ~reader =
  let module Lexxer = Make (struct
      type 'a t = ('a, string) result

      let return v = Ok v
      let bind a f =
        match a with
        | Error _ as err -> err
        | Ok a -> f a
      
      let fail s = Error s

      let read buf len =
        match reader buf len with
        | exception End_of_file -> return 0
        | exception Failure err -> fail err
        | len -> return len

    end)
  in
  fun lexbuf -> Lexxer.token lexbuf

let () =
  let lexbuf = Lexutils.create_lexbuf () in
  let reader buf len = input stdin buf 0 len in
  let lexxer = create_lexxer ~reader in
  let rec loop () = 
    match lexxer lexbuf with
    | Ok EOL -> Printf.printf "EOL "; loop ()
    | Ok (INT i) -> Printf.printf "INT(%d) " i; loop ()
    | Ok PLUS -> Printf.printf "PLUS "; loop ()
    | Ok EOF -> Printf.printf "EOF "
    | Error err -> Printf.printf "\nERROR: %s\n" err
  in
  loop ()
}
