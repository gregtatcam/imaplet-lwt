open Sexplib
open Sexplib.Conv

type t = string [@@deriving sexp];;
include Mimestring.S with type t := t

(* Multitables keyed by Field_names *)
module Assoc_intf : sig
  module type S = sig
    type 'a t
    type field_name    
    val empty : unit -> 'a t;;
    
    val is_empty : 'a t -> bool;;
    val length : 'a t -> int;;

    val add : 'a t -> name:field_name -> body:'a -> 'a t;;
    (** Updates the value of the last field with name. If there is
     no such field, it is appended at the end *)
    val set : 'a t -> name:field_name -> body:'a -> 'a t;;

    val mem : 'a t -> field_name -> bool;;
    val all : 'a t -> field_name -> 'a list;;
    val last : 'a t -> field_name -> 'a option;;

    val names : 'a t -> field_name list;;

    val remove_all : 'a t -> field_name -> 'a t;;
    val remove_last : 'a t -> field_name -> 'a t;;

    (** List of name-body pairs. *)
    val to_list : 'a t -> (field_name * 'a) list;;
    (** Reversed list of name-body pairs. O(1) *)
    val to_rev_list : 'a t -> (field_name * 'a) list;;
  
    (** O(N) *)
    val of_list : (field_name * 'a) list -> 'a t;;
    (** O(1) *)
    val of_rev_list : (field_name * 'a) list -> 'a t;;

    val map_to_list : 'a t -> f:(field_name * 'a -> 'b) -> 'b list
  end with type field_name := t
end 

module Assoc : Assoc_intf.S

(** 
  This is intended to be used as a mixin, so additional functionality
  can be added without rewriting the signature.

  Documentation at the Assoc_intf module.
 *)
module Assoc_concrete (T : Sexpable_.S) : sig
  module type S = sig
    type t = T.t Assoc.t;;
    type field_name

    val empty : t;;

    val is_empty : t -> bool;;
    val length : t -> int;;
    val mem : t -> field_name -> bool;;
    val all : t -> field_name -> T.t list;;
    val last : t -> field_name -> T.t option;;

    val add : t -> name:field_name -> body:T.t -> t;;

    (** Replace or create *)
    val set : t -> name:field_name -> body:T.t -> t;;

   
    val remove_all : t -> field_name -> t;;
    val remove_last : t -> field_name -> t;;


    val names : t -> field_name list;;

    val to_list : t -> (field_name * T.t) list;;

    (** No overhead *)
    val to_rev_list : t -> (field_name * T.t) list;;

    val of_list : (field_name * T.t) list -> t;;

    (** No overhead *)
    val of_rev_list : (field_name * T.t) list -> t;;

    val map_to_list : t -> f:(field_name * T.t -> 'b) -> 'b list

    include Sexpable_.S with type t := t
  end with type field_name := t
  module Make : S
end



