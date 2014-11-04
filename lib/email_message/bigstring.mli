open Bigarray

type t = (char, int8_unsigned_elt, c_layout) Array1.t
with bin_io, sexp

val of_string : ?pos : int -> ?len : int -> string -> t

val to_string : ?pos : int -> ?len : int -> t -> string

val sub_shared : ?pos : int -> ?len : int -> t -> t

val subo : ?pos:int -> ?len:int -> t -> string

val blito : src:t -> ?src_pos:int -> ?src_len:int -> dst:bytes -> ?dst_pos:int -> unit -> unit
