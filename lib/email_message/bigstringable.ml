open Core_replace
module type S = sig
  type t
  val of_bigstring : Bigstring_.t -> t
  val to_bigstring : t -> Bigstring_.t
end
