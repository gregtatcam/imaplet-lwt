open Core_replace

type t = Bigstring_.t

open Bigstring_

let empty = create 0;;

let to_lexbuf t =
  let offset = ref 0 in
  let len = length t in
  Lexing.from_function
    (fun dst n ->
      let read = min n (len - !offset) in
      To_string.blit
        ~src:t
        ~src_pos:!offset
        ~len:read
        ~dst
        ~dst_pos:0
      ;
      offset := !offset + read;
      read)
;;

let foldi t ~init ~f =
  let len = length t in
  let rec loop init pos =
    if pos >= len then
      init
    else
      loop (f pos init t.{pos}) (pos + 1)
  in
  loop init 0
;;

let of_bigbuffer_volatile buffer =
  (* If this isn't done, the buffer might contain extra uninitialized characters *)
  Bigstring_.sub_shared
    ~pos:0
    ~len:(Bigbuffer_.length buffer)
    (Bigbuffer_.volatile_contents buffer)
;;

