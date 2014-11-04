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
open Storage_meta
open Imaplet_types
open Email_message

module type Storage_intf = 
sig
  type t

  (* user,mailbox *)
  val create : string -> string -> t Lwt.t

  (* check if mailbox exists *)
  val exists : t -> [`No|`Folder|`Mailbox] Lwt.t

  (* status *)
  val status : t -> mailbox_metadata Lwt.t

  (* select mailbox *)
  val select : t -> mailbox_metadata Lwt.t

  (* examine mailbox *)
  val examine : t -> mailbox_metadata Lwt.t

  (* create mailbox *)
  val create_mailbox : t -> unit Lwt.t

  (* delete mailbox *)
  val delete : t -> unit Lwt.t

  (* rename mailbox1 mailbox2 *)
  val rename : t -> string -> unit Lwt.t

  (* subscribe mailbox *)
  val subscribe : t -> unit Lwt.t

  (* unsubscribe mailbox *)
  val unsubscribe : t -> unit Lwt.t

  (* list reference mailbox 
   * returns list of files/folders with list of flags 
   *)
  val list : t -> subscribed:bool -> ?access:(string->bool) -> init:'a -> 
    f:('a -> [`Folder of (string*int)|`Mailbox of (string*int)] -> 'a Lwt.t) -> 'a Lwt.t

  (* append message(s) to selected mailbox *)
  val append : t -> Mailbox.Message.t -> mailbox_message_metadata -> unit Lwt.t 

  (* delete a message
   *)
  val delete_message : t -> [`Sequence of int|`UID of int] -> unit Lwt.t

  (* search selected mailbox *)
  val search : t -> (searchKey) searchKeys -> bool -> int list Lwt.t

  (* fetch messages from selected mailbox *)
  val fetch : t -> [`Sequence of int|`UID of int] ->
    [`NotFound|`Eof|`Ok of (Mailbox.Message.t * mailbox_message_metadata)] Lwt.t

  (* fetch messages from selected mailbox *)
  val fetch_message_metadata : t -> [`Sequence of int|`UID of int] ->
    [`NotFound|`Eof|`Ok of mailbox_message_metadata] Lwt.t

  (* store flags to selected mailbox *)
  val store : t -> [`Sequence of int|`UID of int] -> mailbox_message_metadata -> unit Lwt.t

  (* store mailbox metadata *)
  val store_mailbox_metadata : t -> mailbox_metadata -> unit Lwt.t

  (* copy messages from selected mailbox *)
  val copy : t -> t -> sequence -> bool -> unit Lwt.t

  (* copy message from the source mailbox at the given position 
   * to the destination mailbox
   *)
  val copy : t -> [`Sequence of int | `UID of int] -> t ->
    mailbox_message_metadata -> unit Lwt.t

  (* all operations that update the mailbox have to be completed with commit
   *)
  val commit : t -> unit Lwt.t

  (* get sequence # for the given uid *)
  val uid_to_seq : t -> int -> int option Lwt.t
end
module type Storage_inst =
sig
  module MailboxStorage : Storage_intf
  val this : MailboxStorage.t
  val this2 : MailboxStorage.t option
end

let build_strg_inst
(module S : Storage_intf)
user ?mailbox2 mailbox =
  S.create user mailbox >>= fun this ->
  begin
  match mailbox2 with 
  | None -> return None
  | Some mailbox2 -> S.create user mailbox2 >>= fun this2 -> return (Some this2) 
  end >>= fun this2 ->
  return
  (module struct
    module MailboxStorage = S
    let this = this
    let this2 = this2
  end : Storage_inst)
