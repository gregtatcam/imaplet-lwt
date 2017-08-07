open Core_replace

(** Encode text and binary data for inclusion in MIME messages. *)

type t = [
  `Base64 | 
  `Bit7 | 
  `Bit8 | 
  `Binary | 
  `Quoted_printable | 
  `Unknown of string
] [@@deriving sexp]
;;

(** Defaults to mode=`Text *)
include Stringable_.S with type t := t

(** The distinction between text and binary is relevant when Base64
  encoding is used. If the mode is text, '\n' is turned into '\r\n'
  when encoding, and viceversa.
*)
val mode : t -> [ `Text | `Binary ]

val decode : t -> ?media_type:Media_type.t -> Octet_stream.t -> 
  (Octet_stream.t, [`Unknown of string]) Result_.t
val encode : t -> Octet_stream.t -> 
  (Octet_stream.t, [`Unknown of string]) Result_.t

val decode_default : t -> ?media_type:Media_type.t ->
  Octet_stream.t -> Octet_stream.t
val encode_default : t -> Octet_stream.t -> Octet_stream.t

