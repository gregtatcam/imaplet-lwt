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
  irmin_path : string; (* irminsule location, default / *)
  max_msg_size : int;
  imap_name : string; (* greeting name, default imaplet *)
  lmtp_addr : string; (* lmtp server address, default 127.0.0.1 *)
  lmtp_port : int; (* lmtp server port, default 24 *)
  addr : string ref; (* server port, default 127.0.0.1 *)
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

let exists file =
  let stat = Unix.stat file in
  if stat.Unix.st_kind = Unix.S_REG then
    true
  else
    false

let config () =
  if exists Install.config_path then
    Install.config_path
  else
    "./imaplet.cf"

let srv_config = 
  let lines =
    try
      let ic = Pervasives.open_in (config ()) in
      let rec in_lines acc =
        try
          let line = Pervasives.input_line ic in
          in_lines (line :: acc)
        with _ -> acc
      in
      let lines = in_lines [] in
      Pervasives.close_in ic;
      lines
    with _ -> []
  in
  let config = {
    rebuild_irmin = false;
    inbox_path = ref "";(*"/var/mail";*)
    mail_path = ref "";(*"/Users/@/mail";*)
    irmin_path = "/tmp/irmin/test";
    max_msg_size = 0;
    imap_name = "imaplet";
    lmtp_addr = "127.0.0.1";
    lmtp_port = 24;
    addr = ref "127.0.0.1";
    port = ref 993;
    ssl = ref true;
    starttls = ref true;
    data_path = Install.data_path;
    pem_name = "server.pem";
    key_name = "server.key";
    users_path = Install.users_path;
    data_store = ref `Irmin;
    encrypt = true;
    compress = true;
  } in
  let rec proc lines acc =
    match lines with 
    | hd :: tl -> 
    let acc =
    (
    let matched = try 
        Regex.match_regex hd ~regx:"^\\([^# ][^ ]+\\) \\(.+$\\)"
      with _ -> false
    in
    if matched then (
      let n = Str.matched_group 1 hd in
      let v = Str.matched_group 2 hd in
      let v = Regex.replace ~regx:"\"" ~tmpl:"" v in
      let log n v = Printf.printf "%s: invalid value %s\n%!" n v in
      let ival n v default = try int_of_string v with _ -> log n v; default in
      let bval n v default = try bool_of_string v with _ -> log n v; default in
      let stval n = function
        |"irmin"->`Irmin|"mbox"->`Mailbox|"maildir"->`Maildir|_->log n v; `Irmin in
      match n with 
      | "imap_name" -> {acc with imap_name = v }
      | "rebuild_irmin" -> {acc with rebuild_irmin = bval n v false}
      | "inbox_path" -> {acc with inbox_path = ref v}
      | "mail_path" -> {acc with mail_path = ref v}
      | "irmin_path" -> {acc with irmin_path = v}
      | "max_msg_size" -> {acc with max_msg_size = ival n v 10_000_000}
      | "lmtp_addr" -> {acc with lmtp_addr = v}
      | "lmtp_port" -> {acc with lmtp_port = ival n v 24}
      | "addr" -> {acc with addr = ref v}
      | "port" -> {acc with port = ref (ival n v 993)}
      | "ssl" -> {acc with ssl = ref (bval n v true)}
      | "starttls" -> {acc with starttls = ref (bval n v true)}
      | "data_path" -> {acc with data_path = v};
      | "pem_name" -> {acc with pem_name = v}
      | "key_name" -> {acc with key_name = v}
      | "users_path" -> {acc with users_path = v}
      | "data_store" -> {acc with data_store = ref (stval n v)}
      | "encrypt" -> {acc with encrypt = (bval n v true)}
      | "compress" -> {acc with encrypt = (bval n v true)}
      | _ -> Printf.printf "unknown configuration %s\n%!" n; acc
    ) else 
      acc
    ) in proc tl acc
    | [] -> acc
  in
  let config = proc lines config in
  assert ((config.!data_store = `Mailbox && config.!inbox_path <> "" ||
  config.!data_store = `Maildir) && config.!mail_path <> "" ||
  config.!data_store = `Irmin);
  config
