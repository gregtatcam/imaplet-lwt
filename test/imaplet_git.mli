open Lwt

exception InvalidObject

val gitreadt : float ref
val gitcomprt : float ref

module Sha :
sig
  type t
  val of_string : string -> t
  val of_raw : string -> t
  val empty : t
  val prefix : t -> string
  val postfix : t -> string
  val to_string : t -> string
end

type t
type commit =
  {tree:Sha.t;parent:Sha.t list;author:string;committer:string;message:string}
type perm = [`Normal|`Exec|`Link|`Dir|`Commit]
type obj_type = [`Tree|`Blob|`Commit|`Tag]
type tree_entry = {perm:perm;name:string;sha:Sha.t}
type tree = tree_entry list

module Commit :
sig
  type t = commit
  val create : string -> t Lwt.t
  val to_string : t -> string
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
end

val create : repo:string -> t Lwt.t

val read_object : t -> Sha.t -> [`Blob of string|`Tree of tree|`Commit of commit|`Tag of string] Lwt.t

val read_opt : t -> Key.t -> string option Lwt.t

val read_exn : t -> Key.t -> string Lwt.t

val to_string : t -> string
