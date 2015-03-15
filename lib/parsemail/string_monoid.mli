(** Simple library for concatenating immutable strings efficiently *) 

type t

(** Primitive, constant-time operations *)
val empty : t
val nl : t

val plus : t -> t -> t
val length : t -> int

(** Linear in the number of elements. *)
val concat : ?sep:t -> t list -> t

(** Linear in the number of elements in the list. *)
val concat_string : ?sep:string -> string list -> t

(* t_of_* is O(1), *_of_t is O(N), N being the length *)
include Stringable_.S with type t := t
include Bigstringable.S with type t := t

val of_char : char -> t

(*
  For the library to fulfil it's purpose of minimal overhead
  string concatenation, the output functions must be tightly
  coupled with the low-level representation.

  Any new output channel should be implemented as new methods
  of the library itself.
*)
val output_channel : t -> out_channel -> unit

