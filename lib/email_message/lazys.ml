open Core_replace

module Lazy_m : sig
  include module type of Lazy
  val of_fun : (unit -> 'a) -> 'a lazy_t
  val of_val : 'a -> 'a lazy_t
end = struct
  include Lazy
  let of_fun = Lazy.from_fun
  let of_val = Lazy.from_val
end

module Lazy_list : sig
  type 'a t

  val fold_left : f:('a -> 'b -> 'a) -> init:'a -> 'b t -> 'a

  val fold_right : f:('a -> 'b -> 'b) -> 'a t -> init:'b -> 'b

  val iter : 'a t -> f:('a -> unit) -> unit

  val of_iterator : curr:('a -> 'b option) -> next:('a -> 'a) -> init:'a -> 'b t

  val of_list : 'a list -> 'a t

  val to_rev_list : 'a t -> 'a list

  val to_list : 'a t -> 'a list

  val decons : 'a t -> ('a * 'a t) option
end = struct
  type 'a node =
  | Empty
  | Cons of 'a * 'a lazy_list
    and 'a lazy_list = 'a node Lazy_m.t

  type 'a t = 'a lazy_list

  let rec of_list xs = Lazy_m.of_fun (fun () ->
    match xs with
    | [] -> Empty
    | x :: xs -> Cons (x, of_list xs))
  ;;

  let rec fold_left ~f ~init t =
    match Lazy_m.force t with
    | Empty -> init
    | Cons(x, xs) -> fold_left ~f xs ~init:(f init x)
  ;;

  let to_rev_list t = fold_left t ~init:[] ~f:(fun xs x -> x :: xs)

  let to_list t = List.rev (to_rev_list t)

  let fold_right ~f t ~init =
    List.fold_left (fun a b -> f b a) init (to_rev_list t) 
  ;;

  let rec iter t ~f =
    match Lazy_m.force t with
    | Empty -> ()
    | Cons(x, xs) -> f x; iter ~f xs
  ;;

  let of_iterator ~curr ~next ~init =
    let rec loop accum () =
      match curr accum with
      | Some(x) -> Cons(x, Lazy_m.of_fun (loop (next accum)))
      | None -> Empty
      in
      Lazy_m.of_fun (loop init)
  ;;

  let decons t =
    match Lazy_m.force t with
    | Empty -> None
    | Cons(h, t) -> Some(h, t)
  ;;
end

module Lazy_sequence : sig
    type +'a t
    val of_list: 'a list -> 'a t
    val iter: 'a t -> f:('a -> unit) -> unit
    val empty: _ t 
    val read_lines: string -> string t
    val initialize: (unit -> 'a t) -> 'a t
    val (==>): 'a -> (unit -> 'a t) -> 'a t (* Cons *)
    val (==>>): 'a list -> (unit -> 'a t) -> 'a t
    val protect: finally:(unit -> unit) -> (unit -> 'a t) -> 'a t
    val init: (int -> 'a option) -> 'a t
    module Iterator : sig
      type 'a seq = 'a t
      type 'a t
      val create: 'a seq -> 'a t

      val get: 'a t -> 'a option
    end
  end = struct
    type 'a t =
    | Nil
    | Lazy of (unit -> 'a t)
    | Cons of 'a * (unit -> 'a t)
    | Protect of (unit -> unit) * 'a t

    let empty = Nil
    let (==>) x tail = Cons (x, tail)
    let (==>>) lst tail =
      match lst with
      | [] -> Lazy tail
      | lst -> List.fold_right (fun x tail -> (fun () -> Cons (x, tail))) lst tail ()

    let execute_finallys finallys =
      let exns =
        List.filter_map finallys ~f:(fun finally ->
          try finally (); None with exn -> Some exn)
      in
      match List.reduce exns ~f:(fun x y -> Exn.Finally (x,y)) with
      | None -> ()
      | Some exn -> raise exn

    let initialize tail = Lazy tail
    let protect ~finally f = Protect (finally, Lazy f)
    let of_list list =
      let rec of_list list =
        match list with
        | [] -> Nil
        | x :: tail -> x ==> fun () -> of_list tail
        in
        Lazy (fun () -> of_list list)

    let init f =
      let rec loop n =
        match f n with
        | None -> Nil
        | Some x -> x ==> fun () -> loop (n+1)
      in
      Lazy (fun () -> loop 0)

    let add elt listref = listref := elt :: !listref

    let wrap_finallys finallys f =
      Exn.protect ~f ~finally:(fun () -> execute_finallys !finallys)

    let read_lines filename =
      initialize (fun () ->
        let ic = Pervasives.open_in filename in
        protect ~finally:(fun () -> Pervasives.close_in ic) (fun () ->
          let rec loop () =
            try 
              let line = Pervasives.input_line ic in line ==> loop
            with _ -> empty
          in
          loop ()
      ))

    let iter t ~f =
      let finallys = ref [] in
      let rec iter t ~f =
        match t with
        | Nil -> ()
        | Lazy tail -> iter (tail ()) ~f
        | Protect (finally, tail) -> add finally finallys;
          iter tail ~f
        | Cons (x, tail) -> f x; iter (tail ()) ~f
        in
        wrap_finallys finallys (fun () -> iter t ~f)

  module Iterator = struct
    type 'a seq = 'a t

    type 'a shared_data = {
      mutable tail: 'a seq;
      mutable finallys: (unit -> unit) list;
      mutable num_iters: int;
    }

    type 'a node = {
      mutable next: 'a nextnode
    }
    and 'a nextnode =
    | Next of 'a * 'a node
    | Nothing

    type 'a t = {
      mutable node: 'a node;
      shared: 'a shared_data;
      mutable closed: bool;
      mutable stored_node: 'a node;
    }

    let create seq = {
      node = { next = Nothing; };
      shared = {
        tail = seq;
        finallys = [];
        num_iters = 1;
      };
      closed = false;
      stored_node = { next = Nothing; };
    }

    let close_shared shared =
      shared.tail <- Nil;
      let finallys = shared.finallys in
      shared.finallys <- [];
      execute_finallys finallys

    let next_shared shared =
      let rec next shared tail =
        match tail with
        | Nil -> close_shared shared; None
        | Lazy tail ->
          let tail = try tail () with exn -> close_shared shared; raise exn in
          next shared tail
        | Cons (x, tail) -> shared.tail <- Lazy tail; Some x
        | Protect (finally, tail) ->
          shared.finallys <- finally :: shared.finallys;
          next shared tail
      in
      next shared shared.tail

    let get t =
    match t.node.next with
    | Next (x,next) ->
      t.stored_node <- t.node;
      t.node <- next;
      Some x
    | Nothing ->
      if t.closed
      then None
      else begin
        let next = next_shared t.shared in
        (* Performance hack - if only one iterator, we can not bother updating
         * the
         *            linked list for iterators behind us and simply return the
         *            next value *)
        if t.shared.num_iters = 1
        then next
        else
          match next with
          | None -> None
          | Some x ->
            let new_node = { next = Nothing; } in
            t.node.next <- Next (x, new_node);
            t.stored_node <- t.node;
            t.node <- new_node;
            Some x
      end
  end

  end 
