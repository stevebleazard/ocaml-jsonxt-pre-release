{
type token = EOL | INT of int | PLUS | EOF

open Lexing

let fill_lexbuf buf read_len lexbuf =
  let n = if read_len > 0 then read_len else (lexbuf.lex_eof_reached <- true; 0) in
  (* Current state of the buffer:
        <-------|---------------------|----------->
        |  junk |      valid data     |   junk    |
        ^       ^                     ^           ^
        0    start_pos             buffer_end    Bytes.length buffer
  *)
  if lexbuf.lex_buffer_len + n > Bytes.length lexbuf.lex_buffer then begin
    (* There is not enough space at the end of the buffer *)
    if lexbuf.lex_buffer_len - lexbuf.lex_start_pos + n <= Bytes.length lexbuf.lex_buffer
    then 
      (* But there is enough space if we reclaim the junk at the beginning
         of the buffer *)
      Bytes.blit lexbuf.lex_buffer lexbuf.lex_start_pos
                  lexbuf.lex_buffer 0
                  (lexbuf.lex_buffer_len - lexbuf.lex_start_pos)
    else begin
      (* We must grow the buffer.  *)
      let newlen =
        min (max (2 * Bytes.length lexbuf.lex_buffer) n) Sys.max_string_length
      in
      if lexbuf.lex_buffer_len - lexbuf.lex_start_pos + n > newlen then
        failwith "Lexing.lex_refill: cannot grow buffer";
      let newbuf = Bytes.create newlen in
      (* Copy the valid data to the beginning of the new buffer *)
      Bytes.blit lexbuf.lex_buffer lexbuf.lex_start_pos
                  newbuf 0
                  (lexbuf.lex_buffer_len - lexbuf.lex_start_pos);
      lexbuf.lex_buffer <- newbuf
    end;
    (* Reallocation or not, we have shifted the data left by
       start_pos characters; update the positions *)
    let s = lexbuf.lex_start_pos in
    lexbuf.lex_abs_pos <- lexbuf.lex_abs_pos + s;
    lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - s;
    lexbuf.lex_start_pos <- 0;
    lexbuf.lex_last_pos <- lexbuf.lex_last_pos - s;
    lexbuf.lex_buffer_len <- lexbuf.lex_buffer_len - s ;
    let t = lexbuf.lex_mem in
    for i = 0 to Array.length t-1 do
      let v = t.(i) in
      if v >= 0 then
        t.(i) <- v-s
    done
  end;
  (* There is now enough space at the end of the buffer *)
  Bytes.blit buf 0 lexbuf.lex_buffer lexbuf.lex_buffer_len n;
  lexbuf.lex_buffer_len <- lexbuf.lex_buffer_len + n
;;

module Make (M : sig
               type 'a t
               val return: 'a -> 'a t
               val bind: 'a t -> ('a -> 'b t) -> 'b t
               val fail : string -> 'a t

               (* Set up lexbuf *)
               val on_refill : Lexing.lexbuf -> unit t
               (* val read : Bytes.t -> int -> int t *)
             end)
= struct


  let refill_handler k lexbuf =
      M.bind (M.on_refill lexbuf) (fun () -> k lexbuf)

  (*
  let refiller =
    let buf = Bytes.create 512 in
    fun k lexbuf ->
      M.bind (M.read buf (Bytes.length buf)) (fun n -> fill_lexbuf buf n lexbuf; k lexbuf)
  *)
}


refill {refill_handler}

rule token _state = parse 
| [' ' '\t']
    { token _state lexbuf  }
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

let zero_pos = {
  pos_fname = "";
  pos_lnum = 1;
  pos_bol = 0;
  pos_cnum = 0;
}
;;

let create_lexbuf () =
  {
    refill_buff = (fun _lexbuf -> Printf.printf "[refill_buf]%!");
    lex_buffer = Bytes.create 1024;
    lex_buffer_len = 0;
    lex_abs_pos = 0;
    lex_start_pos = 0;
    lex_curr_pos = 0;
    lex_last_pos = 0;
    lex_last_action = 0;
    lex_mem = [||];
    lex_eof_reached = false;
    lex_start_p = zero_pos;
    lex_curr_p = zero_pos;
  }
;;

module Lexxer = Make (struct
    type 'a t = ('a, string) result

    let return v = Ok v
    let bind a f =
      match a with
      | Error _ as err -> err
      | Ok a -> f a

    let fail s = Error s

    let on_refill lexbuf =
      Printf.printf "\non_refill buffer> %!";
      let (line, len) = match input_line stdin with 
        | exception End_of_file -> ("", 0)
        | s -> (s ^ "\n", String.length s + 1)
      in
      fill_lexbuf (Bytes.of_string line) len lexbuf;
      return ()

  end)

let () =
  let lexbuf = create_lexbuf () in
  let rec loop () = 
    match Lexxer.token 1 lexbuf with
    | Ok EOL -> Printf.printf "EOL "; loop ()
    | Ok (INT i) -> Printf.printf "INT(%d) " i; loop ()
    | Ok PLUS -> Printf.printf "PLUS "; loop ()
    | Ok EOF -> Printf.printf "EOF "
    | Error err -> Printf.printf "\nERROR: %s\n" err
  in
  loop ()
}
