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
open Sexplib

val formated_capability : string -> string

val formated_id : string -> string

val to_plist : string -> string

val fl_to_str : Imaplet_types.mailboxFlags -> string

val str_to_fl : string -> Imaplet_types.mailboxFlags

val substr : string -> start:int -> size:(int option)-> string

val concat_path : string -> string -> string

val make_email_message : string -> Email_message.Mailbox.Message.t

val option_value : 'a option -> default:'a -> 'a

val option_value_exn : 'a option -> 'a

val list_find : 'a list -> ('a -> bool) -> bool

val list_findi : 'a list -> (int -> 'a -> bool) -> (int * 'a) option

val with_file : string -> flags:Lwt_unix.open_flag list ->
  perms:Lwt_unix.file_perm -> mode:'a Lwt_io.mode -> 
  f:('a Lwt_io.channel -> 'b Lwt.t) -> 'b Lwt.t 

val exists : string -> Lwt_unix.file_kind -> bool Lwt.t

val lines : string -> int
