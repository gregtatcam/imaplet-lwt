open Sexplib
open Sexplib.Conv

let (|!) a f = f a

module Option_ =
  struct
    let value a ~default =
      match a with
      | None -> default
      | Some a -> a

    let value_exn = function
      | None -> raise (Failure "Option.value_exn None")
      | Some a -> a

    let map a ~f =
      match a with
      |None -> None
      |Some a -> Some (f a)

    let value_map a ~default ~f =
      match a with
      |None -> default
      |Some a -> (f a)

    let try_with f =
      try
        Some (f ())
      with _ -> None

    let iter a ~f =
      match a with
      | None -> ()
      | Some a -> f a

    let is_some o = o <> None

    module Monad_infix =
      struct
        let (>>=) a f =
          match a with 
          | None -> None
          | Some a -> f a

        let (>>|) a f = map a ~f
      end
  end

module Exn : sig
  type t = exn

  exception Finally of t * t
  val protect : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a
end=struct
  type t = exn
  exception Finally of t * t
  let protectx ~f x ~(finally : _ -> unit) =
      let res =
        try f x
        with exn ->
          (try finally x with final_exn -> raise (Finally (exn, final_exn)));
          raise exn
      in
      finally x;
      res

  let protect ~f ~finally = protectx ~f () ~finally
end


module String : sig
  include module type of String
  val t_of_sexp : Sexplib.Sexp.t -> string
  val sexp_of_t : string -> Sexplib.Sexp.t
  val equal : string -> string -> bool
  val hash : string -> int
  val is_empty : string -> bool
  val chop_prefix_exn : string -> prefix:string -> string
  val split : string -> on:char -> string list
  val to_string : string -> string
  val of_string : string -> string
end = struct
  include String
  let t_of_sexp s = Conv.string_of_sexp s
  let sexp_of_t t = Conv.sexp_of_string t
  let equal t1 t2 = (Pervasives.compare t1 t2 = 0)
  let hash s = Hashtbl.hash s
  let is_empty s = (String.length s = 0)
  let chop_prefix_exn s ~prefix =
    let lenpr = length prefix in
    let pr = sub s 0 lenpr in
    if pr = prefix then
      sub s (lenpr - 1) ((length s) - lenpr)  
    else
      raise Not_found
  let split str ~on =
    let rec fold_right (end_,acc) i =
      if i < 0 then
        let item = sub str (i + 1) (end_ - i) in
        item::acc
      else if str.[i] = on then (
        let item = 
          if i = end_ then
            ""
          else
            sub str (i + 1) (end_ - i)
        in
        fold_right (i - 1,item::acc) (i - 1)
      ) else
        fold_right (end_, acc) (i - 1)
    in
    let end_ = (length str) - 1 in
    fold_right (end_,[]) end_
  let to_string t = t
  let of_string s = s
end 

open Bigarray
module Bigstring_ : sig
  type t = (char, int8_unsigned_elt, c_layout) Array1.t 
  module To_string : sig
    val blit : src:t -> src_pos:int -> dst:string -> dst_pos:int -> len:int -> unit
    val blito : src:t -> ?src_pos:int -> ?src_len:int -> dst:string -> ?dst_pos:int ->
     unit -> unit
  end
  module From_string : sig
    val blito : src:string -> ?src_pos:int -> ?src_len:int -> dst:t -> ?dst_pos:int ->
     unit -> unit
  end
  val blito : src:t -> ?src_pos:int -> ?src_len:int -> dst:t -> ?dst_pos:int ->
     unit -> unit
  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
  val sub_shared : ?pos:int -> ?len:int -> t -> t
  val of_string : ?pos : int -> ?len : int -> string -> t
  val to_string : ?pos : int -> ?len : int -> t -> string
  val subo : ?pos:int -> ?len:int -> t -> t
  val length : t -> int
  val create : int -> t
end = struct
  type t = (char, int8_unsigned_elt, c_layout) Array1.t

  let length = Array1.dim

  let create size = Array1.create char c_layout size

  let rec copy f len = function
    | l when l = len -> ()
    | i -> f i; copy f len (i + 1)

  module To_string = struct
    let blit ~src ~src_pos ~dst ~dst_pos ~len =
      copy (fun i -> dst.[dst_pos + i] <- src.{src_pos + i}) len 0
    let blito ~src ?src_pos ?src_len ~dst ?dst_pos () =
      let src_pos = Option_.value src_pos ~default:0 in
      let len = Option_.value src_len ~default:((length src) - src_pos) in
      let dst_pos = Option_.value dst_pos ~default:0 in
      blit ~src ~src_pos ~dst ~dst_pos ~len
  end
  module From_string = struct
    let blito ~src ?src_pos ?src_len ~dst ?dst_pos () =
      let src_pos = Option_.value src_pos ~default:0 in
      let src_len = Option_.value src_len ~default:((String.length src) - src_pos) in
      let dst_pos = Option_.value dst_pos ~default:0 in
      copy (fun i -> dst.{dst_pos + i} <- src.[src_pos + i]) src_len 0
  end

  let blito ~src ?src_pos ?src_len ~dst ?dst_pos () =
    let src_pos = Option_.value src_pos ~default:0 in
    let src_len = Option_.value src_len ~default:((length src) - src_pos) in
    let dst_pos = Option_.value dst_pos ~default:0 in
    copy (fun i -> dst.{dst_pos + i} <- src.{src_pos + i}) src_len 0

  let t_of_sexp s = 
    let l = Conv.list_of_sexp (fun cs -> Conv.char_of_sexp cs) s in
    let t = create (List.length l) in
    List.iteri (fun i c -> t.{i} <- c) l;
    t

  let sexp_of_t t = 
    let len = length t in
    let rec fold_right acc = function
      | l when l = len -> acc
      | i -> fold_right ((t.{len - i - 1}) :: acc) (i + 1)
    in
    Conv.sexp_of_list (fun c -> Conv.sexp_of_char c) (fold_right [] 0)

  let get_opt_len bstr ~pos = function
    | Some len -> len
    | None -> length bstr - pos

  let sub_shared ?(pos = 0) ?len (bstr : t) =
    let len = get_opt_len bstr ~pos len in
    Array1.sub bstr pos len

  let of_string ?pos ?len src = 
    let src_pos = Option_.value pos ~default:0 in
    let src_len = Option_.value len ~default:((String.length src) - src_pos) in
    let dst = Array1.create char c_layout src_len in
    From_string.blito ~src ~src_pos ~src_len ~dst ();
    dst

  let to_string ?pos ?len src = 
    let src_pos = Option_.value pos ~default:0 in
    let src_len = Option_.value len ~default:((length src) - src_pos) in
    let dst = String.create src_len in
    To_string.blito ~src ~src_pos ~src_len ~dst ();
    dst

  let subo ?pos ?len t =
    let pos = Option_.value pos ~default:0 in
    let len = Option_.value len ~default:((length t) - pos) in
    let dst = Array1.create char c_layout len in
    let src = Array1.sub t pos len in
    Array1.blit src dst;
    dst
end

module Bigbuffer_ : sig
  type t [@@deriving sexp_of]
  val create : int -> t
  val contents : t -> string
  val volatile_contents : t -> Bigstring_.t
  val length : t -> int
  val clear : t -> unit
  val add_char : t -> char -> unit
  val add_string : t -> string -> unit
  val add_buffer : t -> t -> unit
end = struct
  type t =
  {
    mutable bstr : Bigstring_.t;
    mutable pos : int;
    mutable len : int;
    init : Bigstring_.t;
  }  [@@deriving sexp_of]


  let resize buf more =
    let min_len = buf.len + more in
    let new_len = min_len + min_len in
    let new_buf = Bigstring_.create new_len in
    Bigstring_.blito ~src:buf.bstr ~src_len:buf.pos ~dst:new_buf ();
    buf.bstr <- new_buf;
    buf.len <- new_len;
  ;;

  let length t = t.pos

  let create n =
   let n = max 1 n in
   let bstr = Bigstring_.create n in
   {
     bstr = bstr;
     pos = 0;
     len = n;
     init = bstr;
   }

  let contents buf = Bigstring_.to_string buf.bstr ~len:buf.pos

  let volatile_contents buf = buf.bstr

  let add_char buf c =
    let pos = buf.pos in
    if pos >= buf.len then resize buf 1;
    buf.bstr.{pos} <- c;
    buf.pos <- pos + 1;
  ;;

  let clear buf = buf.pos <- 0

  let add_string buf src =
    let len = String.length src in
    let new_pos = buf.pos + len in
    if new_pos > buf.len then resize buf len;
    Bigstring_.From_string.blito ~src ~src_len:len ~dst:buf.bstr ~dst_pos:buf.pos ();
    buf.pos <- new_pos;
  ;;

  let add_buffer buf_dst buf_src =
    let len = buf_src.pos in
    let dst_pos = buf_dst.pos in
    let new_pos = dst_pos + len in
    if new_pos > buf_dst.len then resize buf_dst len;
    Bigstring_.blito ~src:buf_src.bstr ~src_len:len ~dst:buf_dst.bstr ~dst_pos ();
    buf_dst.pos <- new_pos;
  ;;
    
end

module Fn_ =
  struct
    let compose f g = (); fun x -> f (g x)

    let id a = a

    let const c = (); fun _ -> c

    let non f = (); fun x -> not (f x)
  end

module Result_ = struct
  type ('ok, 'err) t = 
  | Ok of 'ok
  | Error of 'err

  let of_option o ~error =
    match o with 
    | None -> Error error
    | Some o -> Ok o

  let ok_or_failwith = function
    | Ok ok -> ok
    | Error err -> failwith err

  let ok = function
    | Ok ok -> Some ok
    | Error err -> None

  let map_error r ~f =
    match r with
    | Ok o -> Ok o
    | Error e -> Error (f e)

  module Monad_infix =
    struct
      let (>>|) a f = 
        match a with
        | Error e -> Error e
        | Ok b -> Ok (f b)
      let (>>=) a f =
        match a with
        | Error e -> Error e
        | Ok b -> f b
    end
end

module List : sig 
  include module type of List
  val rev_filter_map : 'a list -> f:('a -> 'b option) -> 'b list
  val filter_map : 'a list -> f:('a -> 'b option) -> 'b list
  val reduce : 'a list -> f:('a -> 'a -> 'a) -> 'a option
  val dedup : ?compare:('a -> 'a -> int) -> 'a list -> 'a list
  val hd : 'a list -> 'a option
  val find_map : 'a list -> f:('a -> 'b option) -> 'b option
  val findi : 'a list -> f:(int -> 'a -> bool) -> (int * 'a) option
  val findi_exn : 'a list -> f:(int -> 'a -> bool) -> (int * 'a)
  module Assoc : sig
    type ('a, 'b) t = ('a * 'b) list
    val mem : ('a, 'b) t -> ?equal:('a -> 'a -> bool) -> 'a -> bool
    val inverse : ('a, 'b) t -> ('b, 'a) t
    val find_exn : ('a, 'b) t -> ?equal:('a -> 'a -> bool) -> 'a -> 'b
    val find : ('a, 'b) t -> ?equal:('a -> 'a -> bool) -> 'a -> 'b option
  end
end = struct
  include List
  let rev_filter_map l ~f =
    List.fold_left (fun acc a -> 
      match f a with
      | Some a -> a::acc
      | None -> acc
    ) [] l

  let filter_map l ~f =
    List.fold_right (fun a acc -> match (f a) with |Some a->a::acc|None->acc) l [];;

  let dedup ?compare l =
    let l =
    match compare with
    | Some compare -> fast_sort compare l
    | None -> fast_sort (fun a1 a2 -> Pervasives.compare a1 a2) l
    in
    let (_,l) =
    fold_right (fun a (prev,acc) ->
      if prev <> None && a = Option_.value_exn prev then
        (prev,acc)
      else
        (Some a,a::acc)
    ) l (None,[])
    in l

  let reduce l ~f =
    match l with
    | [] -> None
    | hd :: [] -> Some hd
    | hd1 :: hd2 :: tl  ->
      let rec reduce_ l acc =
        List.fold_left (fun acc i -> f acc i) acc l
      in Some (reduce_ tl (f hd1 hd2));;

  let hd l =
    try
      Some (List.hd l)
    with _ -> None

  let rec find_map l ~f =
    match l with
    | [] -> None
    | hd :: tl ->
      match f hd with
      | None -> find_map tl ~f
      | Some b -> Some b

  let findi l ~f =
    let rec find_ l i f =
      match l with
      | [] -> None
      | hd :: tl ->
        if f i hd = true then
          Some (i,hd)
        else
          find_ tl (i + 1) f
    in
    find_ l 0 f

  let findi_exn l ~f =
    match findi l ~f with
    | None -> raise Not_found
    | Some (i,a) -> (i,a)

  module Assoc = struct
    type ('a, 'b) t = ('a * 'b) list
    let mem t ?equal a =
      match equal with
      | None -> List.mem_assoc a t
      | Some f -> try let _ = List.find (fun (a1,a2) -> f a1 a) t in true with Not_found -> false
    let inverse t =
      List.fold_right (fun (a,b) acc -> (b,a) :: acc) t [] 
    let find_exn t ?equal a =
      let f =
      match equal with
      | None -> (fun (a1,a2) -> a1 = a)
      | Some f -> (fun (a1,a2) -> f a1 a)
      in
      let (a1,a2) = List.find (fun (a1,a2) -> f (a1,a2)) t in a2
    let find t ?equal a =
      try
        Some (find_exn t ?equal a)
      with Not_found -> None
  end
end
