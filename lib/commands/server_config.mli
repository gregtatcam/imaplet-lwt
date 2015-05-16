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

type imapConfig = {
  rebuild_irmin : bool; (* rebuild irminsule database on start up, default false *)
  inbox_path : string; (* inbox location, default /var/mail *)
  mail_path : string; (* mailboxes location, default /Users/user-name/mail *)
  irmin_path : string; (* irminsule location, default /tmp/irmin/test *)
  irmin_expand : bool; (* expand the contents of the database to the filesystem, default false *)
  max_msg_size : int; (* max email size *)
  imap_name : string; (* greeting name, default imaplet *)
  smtp_addr : string; (* smtp server address, default 127.0.0.1 *)
  smtp_port : int; (* smtp server port, default 24 *)
  smtp_ssl : bool; (* ssl enabled for smtp, default false *)
  smtp_starttls : bool; (* starttls enabled for smtp, default false *)
  addr : string; (* server address, default 127.0.0.1 *)
  port : int; (* server port, default 993 *)
  ssl : bool; (* ssl enabled, default true *)
  starttls : bool; (* starttls enabled, default true *)
  data_path : string; (* pam/key path, default datadir/imaplet *)
  pem_name : string; (* pem file name, default server.pem *)
  key_name : string; (* private key file name, default server.key *)
  users_path : string; (* users file path, default datadir/imaplet *)
  data_store : [`Irmin|`Mailbox|`Maildir]; (* type of storage, only irmin supported so far *)
  encrypt : bool; (* encrypt messages, default true *)
  compress : bool; (* compress messages, but not attachments, default true *)
  user_cert_path : string; (* user's certificate/key location *)
  log : string; (* log location, default /var/log *)
  log_level:[`Error|`Info1|`Info2|`Info3|`Debug|`None]; (* log level, default error *)
  idle_interval: float; (* wait (sec) between idle 'still here' notifications, default 120 sec *)
  smtp_idle_max: float; (* smtp idle time-out, default 300 sec *)
  auth_required: bool; (* require user authentication, priv key encrypted with password, default true *)
}

val default_config : imapConfig

val validate_config : imapConfig -> [`Ok|`Error of string] Lwt.t

val update_config : imapConfig -> string option -> int option -> bool option ->
  bool option -> ([`Irmin|`Mailbox|`Maildir] * string * string) option -> imapConfig

val srv_config : imapConfig

val config_of_lines : string list -> imapConfig
