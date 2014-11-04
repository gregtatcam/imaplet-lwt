open Core_replace

type t = Bigstring_.t

(** Empty, immutable Bigstring_ *)
val empty : t

(** Creates a lexing buffer to be used with the lexing module. *)
val to_lexbuf : t -> Lexing.lexbuf

val foldi : t -> init:'b -> f:(int -> 'b -> char -> 'b) -> 'b

(** Gets a bigstring from a bigbuffer with minimal memory overhead. *)
val of_bigbuffer_volatile : Bigbuffer_.t -> Bigstring_.t



