open Sexplib.Conv
open Core_replace

module type S = sig
  type t = string [@@deriving sexp];;
  val compare : t -> t -> int;;
  val equal : t -> t -> bool;;
  val hash : t -> int;;
end

(** Case-insensitive strings *)
module Case_insensitive = struct
  type t = string [@@deriving sexp]

  let compare x y = String.compare (String.lowercase x) (String.lowercase y)
  let equal x y = String.equal (String.lowercase x) (String.lowercase y)
  let hash x = String.hash (String.lowercase x)
end

let quote_escape =
  let r = Str.regexp "\\([\"\\]\\)" in Str.global_replace r "\\\\\\1"
;;

let quote str = String.concat "" ["\""; quote_escape str; "\""];;
