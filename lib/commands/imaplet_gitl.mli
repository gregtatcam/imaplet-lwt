open Lwt

exception InvalidObject of string
exception InvalidKey

val gitreadt : float ref
val gitcomprt : float ref

module Sha :
sig
  type t
  val of_string : string -> t
  val of_hex_string : string -> t
  val of_hex_binary : string -> t
  val empty : t
  val prefix : t -> string
  val postfix : t -> string
  val to_string : t -> string
end

type t
type commit =
  {tree:Sha.t;parent:Sha.t list;author:string;committer:string;message:string}
type perm = [`Normal|`Exec|`Link|`Dir|`Commit]
type tree_entry = {perm:perm;name:string;sha:Sha.t}
type tree = tree_entry list
type obj_type = [`Tree of tree|`Blob of string|`Commit of commit|`Tag of string|`None]

module Commit :
sig
  type t = commit
  val create : string -> t Lwt.t
  val to_string : t -> string
  val of_values : tree:Sha.t -> parent:Sha.t list -> author:string ->
    committer:string -> message:string -> t
  val to_object_string : t -> string
end

module Key :
sig
  type t = string list
  val create : string list -> t
  val of_unix : string -> t
  val to_string : t -> string
end

module Tree :
sig
  type t = tree
  val create : string -> t
  val to_string : t -> string
  val to_object_string : t -> string
  val update_tree_entry :t -> string -> perm -> Sha.t -> t
  val empty : t
end

val create : repo:string -> t Lwt.t

val mem : t -> ?cache:tree Map.Make(String).t ref -> Key.t -> bool Lwt.t

val read_object : t -> Sha.t -> [`Blob of string|`Tree of tree|`Commit of
commit|`Tag of string|`None] Lwt.t

val read_opt : t -> Key.t -> string option Lwt.t

val read_exn : t -> Key.t -> string Lwt.t

val to_string : t -> string

(* update/create key/v, update all tree objects, return Sha of the witten
 * blob and the root's tree 
 *)
val update : t -> Key.t -> string -> (Sha.t*Sha.t) Lwt.t

(* commit root's tree sha, return updated gitl *)
val commit : t -> Sha.t -> author:string -> message:string -> t Lwt.t

(* combines update and commit, return sha of updated object and updated gitl *)
val update_with_commit : t -> author:string -> message:string -> Key.t -> string -> (Sha.t*t) Lwt.t

val pretty_log : t -> string list Lwt.t
