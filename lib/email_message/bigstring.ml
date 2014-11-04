open Bigarray

type t = (char, int8_unsigned_elt, c_layout) Array1.t
with bin_io, sexp

val of_string : ?pos : int -> ?len : int -> string -> t

val to_string : ?pos : int -> ?len : int -> t -> string

val subo : ?pos:int -> ?len:int -> t -> string

val blito : src:t -> ?src_pos:int -> ?src_len:int -> dst:bytes -> ?dst_pos:int -> unit -> unit

let get_opt_len bstr ~pos = function
  | Some len -> len
  | None -> length bstr - pos

let sub_shared ?(pos = 0) ?len (bstr : t) =
  let len = get_opt_len bstr ~pos len in
  Array1.sub bstr pos len
