open Sexplib.Conv
open Core_replace

type t = {
  mime_type : string;
  mime_subtype : string;
  params : string Field_name.Assoc.t;
} with fields, sexp

val is_multipart : t -> bool
val is_digest : t -> bool

val is_message_rfc2822 : t -> bool

val is_simple : t -> bool

(** A composite message may be decomposed into several submessages *)
val is_composite : t -> bool

val mode : t -> [ `Text | `Binary ]

val multipart_boundary : t -> Boundary.t option

include Stringable_.S with type t := t

