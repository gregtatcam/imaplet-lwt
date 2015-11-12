(*
 * Copyright (c) 2013-2016 Gregory Tsipenyuk <gregtsip@cam.ac.uk>
 * 
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Lwt

exception InvalidObject of string
exception InvalidKey
exception InvalidArgument of string
exception InvalidCommit

val gitreadt : float ref
val gitcomprt : float ref

module Sha :
sig
  type t
  (* sha is a 20 bytes hex string *)
  val of_hex_string : string -> t
  (* sha is binary *)
  val of_hex_binary : string -> t
  (* calculate sha from the content *)
  val of_string : string -> t
  val empty : t
  val prefix : t -> string
  val postfix : t -> string
  val to_string : t -> string
  val to_binary : t -> string
  val equal : t -> t -> bool
  val is_empty : t -> bool
end

type t
type commit =
  {tree:Sha.t;parent:Sha.t list;author:string;committer:string;message:string}
type perm = [`Normal|`Exec|`Link|`Dir|`Commit]
type tree_entry = {perm:perm;name:string;sha:Sha.t}
type tree = tree_entry ref list
type obj_type = [`Tree of tree|`Blob of string|`Commit of commit|`Tag of string|`None]
type cache_type = (string * Sha.t * tree) Map.Make(String).t ref

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
  val to_string : ?absolute_path:bool -> t -> string
  val to_string_rev : t -> string
  val parent : t -> t
  val replace_wild : t -> Sha.t -> t
  val add : t -> string -> t
  val last : t -> string
  val is_empty : t -> bool
  val equal : t -> t -> bool
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

module Object :
sig
  type t
  val create : ?compress:int -> string -> t
  val read : t -> Sha.t -> obj_type Lwt.t
  val write : t -> [`Blob of string|`Tree of Tree.t|
    `Commit of Commit.t|`Tag of string] -> Sha.t Lwt.t
  val read_raw : t -> Sha.t -> string Lwt.t
  val write_raw : t -> ?sha:Sha.t -> string -> Sha.t Lwt.t
  val blob_sha : string -> Sha.t
  val tree_sha : tree -> string * Sha.t
  val exists : t -> Sha.t -> bool Lwt.t
  val file_from_sha : t -> Sha.t -> string
end

val create : ?compress:int -> repo:string -> unit -> t Lwt.t

val mem : t -> Key.t -> bool Lwt.t

val read_object : t -> Sha.t -> [`Blob of string|`Tree of tree|`Commit of
commit|`Tag of string|`None] Lwt.t

val read_opt : t -> Key.t -> string option Lwt.t

val read_exn : t -> Key.t -> string Lwt.t

val to_string : t -> string

(* update/create key/v, update all tree objects, return Sha of the witten
 * blob and the root's tree 
 *)
val update : t -> Key.t -> string -> (Sha.t*Sha.t) Lwt.t

val remove : t -> Key.t -> Sha.t Lwt.t

val rename : t -> src:Key.t -> dest:Key.t -> Sha.t Lwt.t

(* commit return updated gitl *)
val commit : t -> author:string -> message:string -> t Lwt.t

val list : t -> ?filter_blob:bool -> Key.t -> Key.t list Lwt.t

val pretty_log : t -> string list Lwt.t

val update_tree : t -> Key.t -> 
  [`Rename of string*string|`Delete of string|`Add of string * perm * Sha.t] -> Sha.t Lwt.t

val find_sha_opt : t -> Key.t -> Sha.t option Lwt.t

val find_sha_exn : t -> Key.t -> Sha.t Lwt.t

val find_obj_opt : t -> ?leaf:bool -> Key.t -> obj_type Lwt.t

val write_ : ?preempt:bool -> ?compress:int -> file:string -> string -> unit Lwt.t

val read_ : ?compress:int -> ?preempt:bool -> string -> Mstruct.t Lwt.t
