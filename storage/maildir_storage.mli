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
open Storage

module MaildirPath : sig 
  type t
  val create : string -> string -> t
  val to_maildir : t -> string
  val to_unix : t -> string
  val to_unix_path : string -> string
  val file_path : t -> [`Cur of string|`Tmp of string|`New of
  string|`Metadata|`Uidlist|`Keywords] -> string
  val file_path_of_maildir : t -> string -> [`Cur of string|`Tmp of string|`New of
  string|`Metadata|`Uidlist|`Keywords] -> string
  val basename_of_maildir : string -> string
  val basename : t -> string
  val dirname_of_maildir : string -> string
  val dirname : t -> string
  val mailbox : t -> string
  val root : t -> string
  val trim_mailbox : t -> string -> string
end

val message_file_name_to_data : string -> string ->
  (Dates.ImapTime.t*int*int64*Imaplet_types.mailboxFlags list)

val write_mailbox_metadata : string -> Storage_meta.mailbox_metadata -> unit Lwt.t

val write_uidlist : string -> (int*string) list -> unit Lwt.t

val append_uidlist : string -> int -> string -> unit Lwt.t

val flags_of_map_str : string -> string -> Imaplet_types.mailboxFlags list

val make_message_file_name : string -> Storage_meta.mailbox_message_metadata -> string

val create_file : ?overwrite:bool -> ?perms:int -> string -> unit Lwt.t

type storage_ = {user: string; mailbox: MaildirPath.t}

module MaildirStorage : Storage_intf with type t = storage_
