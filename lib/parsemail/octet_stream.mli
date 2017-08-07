open Sexplib.Conv
open Core_replace

type t [@@deriving sexp]

(** 
  These are the most efficient functions to convert to/from octet_streams.
 *)

(*
  This is the preferred way to create
    `mode' is `Text by default. 
    fix_win_eol is false by default. 
    If fix_win_eol is true, and `mode' is `Text, all CRLF pairs will
      be converted to LF in place.

*)
val create : ?mode:[`Text | `Binary] -> ?fix_win_eol:bool -> Bigstring_.t -> t
val contents : t -> Bigstring_.t

(** mode defaults to `Text *)
include Stringable_.S with type t := t
include Bigstringable.S with type t := t

include String_monoidable.S with type t := t
include Lexable.S with type t := t

val of_string_monoid : String_monoid.t -> t

val empty : t
val length : t -> int

val mode_set : t -> [`Text | `Binary] -> t
val mode : t -> [`Text | `Binary]

val is_text : t -> bool
val is_binary : t -> bool

val sub : ?pos:int -> ?len:int -> t -> t

(** RFC2045 compliant base64 *)
module Base64 : sig
  val decode : ?mode:[`Text | `Binary] -> t -> t
  val encode : t -> t
end

(** RFC2045 compliant Quoted-printable *)
module Quoted_printable : sig
  val decode : ?mode:[`Text | `Binary] -> t -> t
  val encode : t -> t
end

module Identity : sig
  val decode : ?mode:[`Text | `Binary] -> t -> t
  val encode : t -> t
end
