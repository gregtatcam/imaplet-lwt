(*
 * Copyright (c) 2013-2014 Gregory Tsipenyuk <gregtsip@cam.ac.uk>
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
open Imaplet_types
open Storage_meta

type t

exception InvalidStorageType

type selection = [`Select of string | `Examine of string | `None]

type amailboxt = 
  {inbox_path:string;mail_path:string;user:string
  option;user_with_domain:string;selected:selection;config:Server_config.imapConfig;
  keys:(Ssl_.keys Lwt.t) option}

val create : Server_config.imapConfig -> string -> string option -> t

(** create empty mailbox **)
val empty : unit -> t 

(** mailboxes, parent folder, mailbox [wildcards] -> list of mailboxes/flags **)
val list : t -> string -> string -> ((string*string list) list) Lwt.t

(** mailboxes, parent folder, mailbox [wildcards] -> list of subscribed mailboxes/flags **)
val lsub : t -> string -> string -> ((string*string list) list) Lwt.t

(** select the mailbox **)
val select : t ->  string -> ([`NotExists|`NotSelectable|`Ok of t * mailbox_metadata] Lwt.t)

(** select the mailbox **)
val examine : t ->  string -> ([`NotExists|`NotSelectable|`Ok of t * mailbox_metadata] Lwt.t)

(** un-select the mailbox **)
val close : t -> t

(** create mailbox **)
val create_mailbox : t -> string -> [`Error of string|`Ok] Lwt.t

(** delete mailbox **)
val delete_mailbox : t -> string -> [`Error of string|`Ok] Lwt.t

(** rename mailbox **)
val rename_mailbox : t -> string -> string -> [`Error of string|`Ok] Lwt.t

(** subscribe mailbox **)
val subscribe : t -> string -> [`Error of string|`Ok] Lwt.t

(** subscribe mailbox **)
val unsubscribe : t -> string -> [`Error of string|`Ok] Lwt.t

(** append message to the mailbox **)
val append : t -> string -> Lwt_io.input_channel -> Lwt_io.output_channel -> 
  mailboxFlags list option -> Dates.ImapTime.t option ->
  literal -> ([`NotExists|`NotSelectable|`Eof of int|`Error of string|`Ok] Lwt.t)

(** sarch messages for the matching criteria **)
val search : t -> (unit -> unit Lwt.t) -> (searchKey) searchKeys -> bool ->
  [`NotExists|`NotSelectable|`Error of string|`Ok of (int64 option * int list)] Lwt.t

val fetch : t -> (unit -> unit Lwt.t) -> (string->unit Lwt.t) -> sequence ->
  fetch -> int64 option -> bool -> [`NotExists|`NotSelectable|`Error of string|`Ok] Lwt.t

val store : t -> (unit -> unit Lwt.t) -> (string->unit Lwt.t) -> sequence -> storeFlags -> mailboxFlags list -> 
  Int64.t option -> bool -> [`NotExists|`NotSelectable|`Error of string|`Ok of string list] Lwt.t

val copy : t -> string -> sequence -> bool -> 
[`NotExists|`NotSelectable|`Error of string|`Ok ] Lwt.t 

val expunge : t -> (string->unit Lwt.t) -> [`NotExists|`NotSelectable|`Error of string|`Ok] Lwt.t 

val user : t -> string option

val user_with_domain : t -> string

val selected_mbox : t -> string option
