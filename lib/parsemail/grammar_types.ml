
(* Some simple, lightweight types for parser output *)
type header = (Field_name.t * string) list;;


type content_offset = [`Content_offset of int | `Truncated];;
type message = [`Message of (header * content_offset)];;

(* Field types *)
type content_type = (string * string * ((Field_name.t * string) list));;
