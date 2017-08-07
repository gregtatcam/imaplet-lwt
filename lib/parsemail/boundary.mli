type t [@@deriving sexp]

(** Creates a boundary from the value of the "boundary" parameter in a
  Content-type header (RFC2046, p.19)
  Alias of to_string.
  *)
val create : string -> t

(** Splits an multipart body into a list of messages, and, if there are,
  an optional prologue and epilogue. *)
val split_octet_stream : t -> octet_stream:Octet_stream.t -> 
  ( Octet_stream.t option *
    Octet_stream.t list * 
    Octet_stream.t option)

(** Creates a valid boundaries for an octet stream. *)
val generate : ?text:Octet_stream.t -> ?suggest:t -> unit -> t
val generate_list : ?text:Octet_stream.t -> unit -> t Lazys.Lazy_sequence.t

(** Open an close boundaries *)

(** Used when the boundary indicates a new part *)
module Open : String_monoidable.S with type t := t

(** Used when the boundary indicates that there are no more parts *) 
module Close : String_monoidable.S with type t := t

(** Used when the boundary indicates the beginning of the first
  part of the message, and there is no prologue. *)
module Open_first : String_monoidable.S with type t := t

include Stringable_.S with type t := t


