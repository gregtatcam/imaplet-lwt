open Core_replace

(** Objects which can be converted to and from Bigstring.t. The following
  holds for all modules implementing this interface.

    (to_bigstring (of_bigstring (to_bigstring t)) = to_bigstring t

    If the object also implements Stringable, the following should hold:

    Bigstring.to_string (to_bigstring t) = to_string t
*)
module type S = sig
  type t

  (** 
    Creates an object of type t from a Bigstring.t. The source Bigstring
    might not be copied, so it should be copied if the need to mutate it 
    arises. The latter does not apply if the implementation of to_bigstring
    explicitly says that this is unnecessary.
  *)
  val of_bigstring : Bigstring_.t -> t

  (** Bigstring representation of an object of type t. t may hold references
    to the resulting Bigstring, so it should be copied if the need to
    mutate it arises. The latter does not apply if the implementation
    explicitly says that it is unnecessary. 
    *)
  val to_bigstring : t -> Bigstring_.t
end
