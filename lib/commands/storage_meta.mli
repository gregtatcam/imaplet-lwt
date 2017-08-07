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
open Sexplib.Std
open Imaplet_types

type mailbox_metadata = {
  uidvalidity: string;
  uidnext: int;
  modseq: int64;
  count: int;
  unseen: int;
  nunseen: int;
  recent: int;
  selectable: bool;
} [@@deriving sexp]

type mbox_mailbox_metadata = {
  mbox_metadata: mailbox_metadata;
  uids: int list;
}

type mailbox_message_metadata = {
  uid: int;
  modseq: int64;
  internal_date: Dates.ImapTime.t;
  size: int;
  flags: mailboxFlags list;
} [@@deriving sexp]

type mbox_message_metadata = {
  metadata: mailbox_message_metadata;
  start_offset: int64;
  end_offset: int64;
}

type mbox_index = [`Header of mbox_mailbox_metadata | `Record of mbox_message_metadata]

val empty_mailbox_metadata : ?uidvalidity:string -> ?selectable:bool -> unit -> mailbox_metadata

val empty_mailbox_message_metadata : unit -> mailbox_message_metadata

val empty_mbox_message_metadata : unit -> mbox_message_metadata

val update_mailbox_metadata: header:mailbox_metadata -> ?uidvalidity:string ->
  ?uidnext:int -> ?modseq:int64 -> ?count:int -> ?unseen:int ->
    ?nunseen:int ->
    ?recent:int -> ?selectable:bool -> unit -> mailbox_metadata

val update_mailbox_message_metadata : data:mailbox_message_metadata ->
  ?uid:int -> ?modseq:int64 ->
  ?internal_date:Dates.ImapTime.t -> ?size:int ->
  ?flags:(mailboxFlags list) -> unit -> mailbox_message_metadata

val update_mbox_message_metadata : data:mbox_message_metadata -> ?uid:int -> ?modseq:int64 ->
  ?internal_date:Dates.ImapTime.t -> ?size:int ->
  ?start_offset:int64 -> ?end_offset:int64 -> ?flags:(mailboxFlags list) -> unit
  -> mbox_message_metadata

val new_uidvalidity : unit -> string

