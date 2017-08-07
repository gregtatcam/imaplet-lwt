open Sexplib.Conv

(* For usage in functors *)
module type S = sig
  type t = string [@@deriving sexp];;
  val compare : t -> t -> int;;
  val equal : t -> t -> bool;;
  val hash : t -> int;;
end

module Case_insensitive : S

val quote : string -> string


