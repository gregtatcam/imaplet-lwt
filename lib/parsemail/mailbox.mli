open Sexplib
open Core_replace
open Lazys

module Postmark : sig
  type t = {
    from : string;
    time : float;
  } [@@deriving sexp]

  val to_string : t -> string
  val of_string : string -> t
end

module Message : sig
  type t = {
    postmark : Postmark.t;
    email : Email.t;
  } [@@deriving sexp]
  val to_string : t -> string;;
end


module type With_container = sig
  type t

  val t_of_fd : in_channel -> t
  val t_of_file : string -> t
  val of_string : string -> t
  val iter_string : t -> f:(string -> unit) -> unit
  val fold_message : t -> f:('a -> Message.t -> 'a) -> init:'a -> 'a
end

module With_lazy_list : With_container with type t = Message.t Lazy_list.t;;

(**
  Warning: When created from a file descriptor, the resulting mailbox can
  only be traversed once.
*)
module With_seq : With_container with type t = Message.t Lazy_sequence.t;;
