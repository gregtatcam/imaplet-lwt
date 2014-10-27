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
open Email_message
open Imaplet_types
open Storage_meta

exception InvalidSequence

val exec_search : Email.t -> (searchKey) searchKeys -> mailbox_message_metadata -> int -> bool

val get_sequence : string -> (seq_set list)

val exec_fetch : int -> sequence -> Mailbox.Message.t ->
  mailbox_message_metadata -> fetch -> int64 option -> bool -> string option

val exec_store : mailbox_message_metadata->int -> sequence ->
  storeFlags -> mailboxFlags list -> Int64.t option -> bool -> 
  [`Ok of mailbox_message_metadata*string|`Silent of mailbox_message_metadata|`Modseqfailed of int |`None]

val exec_seq : sequence -> int -> bool

val check_search_seq : (searchKey) searchKeys -> seq:int -> uid:int -> bool

val has_key : (searchKey) searchKeys -> (searchKey -> bool) -> bool
