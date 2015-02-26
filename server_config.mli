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

type imapConfig = {
  rebuild_irmin : bool; (* rebuild irminsule database on start up, default false *)
  inbox_path : string ref; (* inbox location, default /var/mail *)
  mail_path : string ref; (* mailboxes location, default /Users/user-name/mail *)
  irmin_path : string; (* irminsule location, default /tmp/irmin/test *)
  irmin_expand : bool; (* expand the contents of the database to the filesystem, default false *)
  max_msg_size : int; (* max email size *)
  imap_name : string; (* greeting name, default imaplet *)
  lmtp_addr : string; (* lmtp server address, default 127.0.0.1 *)
  lmtp_port : int; (* lmtp server port, default 24 *)
  addr : string ref; (* server address, default 127.0.0.1 *)
  port : int ref; (* server port, default 993 *)
  ssl : bool ref; (* ssl enabled, default true *)
  starttls : bool ref; (* starttls enabled, default true *)
  data_path : string; (* pam/key path, default datadir/imaplet *)
  pem_name : string; (* pem file name, default server.pem *)
  key_name : string; (* private key file name, default server.key *)
  users_path : string; (* users file path, default datadir/imaplet *)
  data_store : [`Irmin|`Mailbox|`Maildir] ref; (* type of storage, only irmin supported so far *)
  encrypt : bool; (* encrypt messages, default true *)
  compress : bool; (* compress messages, but not attachments, default true *)
}

val srv_config : imapConfig
