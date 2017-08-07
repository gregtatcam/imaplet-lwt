open Sexplib.Conv
open Core_replace

module Field_name = struct
  type t = string [@@deriving sexp]
  include (Mimestring.Case_insensitive : Mimestring.S with type t := t)
end

(* Some utility functions *)
let is_named x = (fun (a,_) -> Field_name.equal a x);;

(** Case insensitive, immutable multimap *)
let split_rev l ~f =
  let rec split_rev' l1 l2 =
    match l2 with
    | x :: xs -> if f x then (l1, l2) else split_rev' (x :: l1) xs
    | []      -> (l1,l2)
  in
  split_rev' [] l
;;

module Assoc_intf = struct
  module type S = sig
    type 'a t;;

    val empty : unit -> 'a t;;

    val is_empty : 'a t -> bool;;
    val length : 'a t -> int;;

    val add : 'a t -> name:Field_name.t -> body:'a -> 'a t;;
    val set : 'a t -> name:Field_name.t -> body:'a -> 'a t;;

    val mem : 'a t -> Field_name.t -> bool;;

    val all : 'a t -> Field_name.t -> 'a list;;
    val last : 'a t -> Field_name.t -> 'a option;;

    val names : 'a t -> Field_name.t list;;

    val remove_all : 'a t -> Field_name.t -> 'a t;;
    val remove_last : 'a t -> Field_name.t -> 'a t;;

    val to_list : 'a t -> (Field_name.t * 'a) list;;
    val to_rev_list : 'a t -> (Field_name.t * 'a) list;;
    val of_list : (Field_name.t * 'a) list -> 'a t;;
    val of_rev_list : (Field_name.t * 'a) list -> 'a t;;

    (** General list functions *)
    val map_to_list : 'a t -> f:(Field_name.t * 'a -> 'b) -> 'b list
  end
end

module Assoc : Assoc_intf.S = struct
  type 'a t = (Field_name.t * 'a) list;;

  let empty () = [];;

  let length = List.length;;
  let is_empty = fun l -> List.length l = 0;;

  let add t ~name ~body = (name, body) :: t;;

  let mem t name = List.Assoc.mem ~equal:Field_name.equal t name;;

  let all t name = List.rev_filter_map t
    ~f:(fun (name', body) ->
      if Field_name.equal name name' then Some body else None
    )
  ;;

  let last t name = List.Assoc.find ~equal:Field_name.equal t name;;

  let names t =
      (List.dedup
        ~compare:Field_name.compare
        (List.map fst t))
  ;;

  let remove_all t name = List.filter
    (Fn_.non (is_named name)) t;;

  let update_last t name ~f =
    let a, b = split_rev t ~f:(is_named name) in
    match b with
    | []                 -> raise Not_found
    | field :: xs ->
      assert (is_named name field);
      List.rev_append a ((f field) @ xs)
  ;;

  let remove_last t name = update_last t name ~f:(Fn_.const []);;

  let set t ~name ~body =
    try
      update_last t name ~f:(Fn_.const [(name, body)])
    with
    Not_found -> add t ~name ~body
  ;;

  let __UNUSED_VALUE__flatten ?(f=(Fn_.const true)) t =
    let rec flatten' ~acc = function
      | []  -> acc
      | ((name, _) as field) :: fs ->
        if f name then
          flatten' ~acc:(field :: acc) (remove_all fs name)
        else
          flatten' ~acc:(field :: acc) fs
    in
    List.rev (flatten' ~acc:[] t)

  (** Special care must be taken because the list is reversed *)
  let to_list t = List.rev t;;
  let to_rev_list = Fn_.id;;

  let of_list t = List.rev t;;
  let of_rev_list = Fn_.id;;

  let map_to_list t ~f = List.rev_map f (to_rev_list t);;
end

module Assoc_concrete (T : Sexpable_.S) = struct
  module type S = sig
    type t = T.t Assoc.t;;

    val empty : t;;

    val is_empty : t -> bool;;
    val length : t -> int;;
    val mem : t -> Field_name.t -> bool;;

    val all : t -> Field_name.t -> T.t list;;
    val last : t -> Field_name.t -> T.t option;;

    val add : t -> name:Field_name.t -> body:T.t -> t;;
    val set : t -> name:Field_name.t -> body:T.t -> t;;
    val remove_all : t -> Field_name.t -> t;;
    val remove_last : t -> Field_name.t -> t;;

    val names : t -> Field_name.t list;;

    val to_list : t -> (Field_name.t * T.t) list;;
    val to_rev_list : t -> (Field_name.t * T.t) list;;

    val of_list : (Field_name.t * T.t) list -> t;;
    val of_rev_list : (Field_name.t * T.t) list -> t;;

    val map_to_list : t -> f:(Field_name.t * T.t -> 'b) -> 'b list

    include Sexpable_.S with type t := t
  end

  module Make = struct
    type 'a z = 'a Assoc.t
    type t = T.t z

    module As_list = struct
      type t = (Field_name.t * T.t) list [@@deriving sexp]
    end

    include (Assoc : Assoc_intf.S with type 'a t := 'a z)
    let empty = empty ()

    let t_of_sexp sexp = of_list (As_list.t_of_sexp sexp)
    let sexp_of_t t = (As_list.sexp_of_t (to_list t))
  end
end

include Field_name
