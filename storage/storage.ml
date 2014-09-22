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
open Core.Std
open Storage_meta
open Imaplet_types
open Email_message
open Email_message.Mailbox.Message

module type Storage_intf = 
sig
  type t

  (* user *)
  val create : string -> t

  (* status *)
  val status : t -> string -> mailbox_metadata Lwt.t

  (* select mailbox *)
  val select : t -> string -> mailbox_metadata Lwt.t

  (* examine mailbox *)
  val examine : t -> string -> mailbox_metadata Lwt.t

  (* create mailbox *)
  val create_mailbox : t -> string -> unit Lwt.t

  (* delete mailbox *)
  val delete : t -> string -> unit Lwt.t

  (* rename mailbox1 mailbox2 *)
  val rename : t -> string -> string -> unit Lwt.t

  (* subscribe mailbox *)
  val subscribe : t -> string -> unit Lwt.t

  (* unsubscribe mailbox *)
  val unsubscribe : t -> string -> unit Lwt.t

  (* list reference mailbox 
   * returns list of files/folders with list of flags 
   *)
  val list : t -> string -> string -> init:'a -> 
    f:('a -> [`Folder of (string*int)|`Mailbox of string] -> ('a*bool) Lwt.t) -> 'a Lwt.t

  (* lsub reference mailbox - list of subscribed mailboxes
   * returns list of files/folders with list of flags 
   *)
  val lsub : t -> string -> string -> init:'a -> 
    f:('a -> [`Folder of (string*int)|`Mailbox of string] -> ('a*bool) Lwt.t) -> 'a Lwt.t

  (* append message(s) to selected mailbox *)
  val append : t -> string -> Mailbox.Message.t -> mailbox_message_metadata -> unit Lwt.t 

  (* expunge, permanently delete messages with \Deleted flag 
   * from selected mailbox 
   *)
  val expunge : t -> string -> (int -> unit Lwt.t) -> unit Lwt.t

  (* search selected mailbox *)
  val search : t -> string -> (searchKey) searchKeys -> bool -> int list Lwt.t

  (* fetch messages from selected mailbox *)
  val fetch : t -> string -> [`Sequence of int|`UID of int] ->
    [`NotFound|`Eof|`Ok of (Mailbox.Message.t * mailbox_message_metadata)] Lwt.t

  (* store flags to selected mailbox *)
  val store : t -> string -> [`Sequence of int|`UID of int] -> mailbox_message_metadata -> unit Lwt.t

  (* copy messages from selected mailbox *)
  val copy : t -> string -> string -> sequence -> bool -> unit Lwt.t
end

module type Storage_inst =
sig
  module MailboxStorage : Storage_intf
  val this : MailboxStorage.t
end

let build_strg_inst
(type l)
(module S : Storage_intf with type t = l) 
user =
  (module struct
    module MailboxStorage = S
    let this = S.create user
  end : Storage_inst)
