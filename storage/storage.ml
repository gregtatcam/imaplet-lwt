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

module type Storage_intf = 
sig
  type t

  (* user *)
  val create : string -> t

  (* status *)
  val status : t -> string -> [`NotExists|`NotSelectable|`Selected of mailbox_metadata] Lwt.t

  (* select mailbox *)
  val select : t -> string -> [`NotExists|`NotSelectable|`Selected of mailbox_metadata] Lwt.t

  (* examine mailbox *)
  val examine : t -> string -> [`NotExists|`NotSelectable|`Selected of mailbox_metadata] Lwt.t

  (* create mailbox *)
  val create_mailbox : t -> string -> [`Error of string|`Ok] Lwt.t

  (* delete mailbox *)
  val delete : t -> string -> [`Error of string|`Ok] Lwt.t

  (* rename mailbox1 mailbox2 *)
  val rename : t -> string -> string -> [`Error of string|`Ok] Lwt.t

  (* subscribe mailbox *)
  val subscribe : t -> string -> [`Error of string|`Ok] Lwt.t

  (* unsubscribe mailbox *)
  val unsubscribe : t -> string -> unit Lwt.t

  (* list reference mailbox 
   * returns list of files/folders with list of flags 
   *)
  val list : t -> string -> string -> (string * string list) list Lwt.t

  (* lsub reference mailbox - list of subscribed mailboxes
   * returns list of files/folders with list of flags 
   *)
  val lsub : t -> string -> string -> (string * string list) list Lwt.t

  (* append message(s) to selected mailbox *)
  val append : t -> string -> Lwt_io.input_channel -> Time.t option -> mailboxFlags list option -> 
    literal -> [`NotExists|`NotSelectable|`Eof|`Error of string|`Ok] Lwt.t

  (* expunge, permanently delete messages with \Deleted flag 
   * from selected mailbox 
   *)
  val expunge : t -> string -> (int -> unit Lwt.t) -> unit Lwt.t

  (* search selected mailbox *)
  val search : t -> string -> (int -> unit Lwt.t) -> (searchKey) searchKeys -> bool ->
    [`NotExists|`NotSelectable|`Error of string|`Ok] Lwt.t

  (* fetch messages from selected mailbox *)
  val fetch : t -> string -> (string -> unit Lwt.t) -> sequence -> fetch -> bool ->
    [`NotExists|`NotSelectable|`Error of string|`Ok] Lwt.t

  (* store flags to selected mailbox *)
  val store : t -> string -> (string -> unit Lwt.t) -> sequence -> storeFlags ->
    mailboxFlags list -> bool -> [`NotExists|`NotSelectable|`Error of string|`Ok] Lwt.t

  (* copy messages from selected mailbox *)
  val copy : t -> string -> string -> sequence -> bool -> [`NotExists|`NotSelectable|`Error of string|`Ok] Lwt.t
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
