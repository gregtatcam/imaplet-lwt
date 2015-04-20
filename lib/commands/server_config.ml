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
  max_msg_size : int;
  imap_name : string; (* greeting name, default imaplet *)
  lmtp_addr : string; (* lmtp server address, default 127.0.0.1 *)
  lmtp_port : int; (* lmtp server port, default 24 *)
  addr : string; (* server port, default 127.0.0.1 *)
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
}

let default_config = {
  rebuild_irmin = false;
  inbox_path = "";(*"/var/mail";*)
  mail_path = "";(*"/Users/@/mail";*)
  irmin_path = "/var/mail/accounts/%user%";
  irmin_expand = false;
  max_msg_size = 0;
  imap_name = "imaplet";
  lmtp_addr = "127.0.0.1";
  lmtp_port = 24;
  addr = "127.0.0.1";
  port = 993;
  ssl = true;
  starttls = true;
  data_path = Install.data_path;
  pem_name = "server.pem";
  key_name = "server.key";
  users_path = Install.users_path;
  data_store = `Irmin;
  encrypt = true;
  compress = true;
}

let validate_config config =
  let err res msg =
    begin
    if res = false then (
      return (`Error (Printf.sprintf "%s %s\n%!" msg Install.config_path))
    ) else
      return `Ok
    end
  in
  match config.data_store with
  | `Irmin -> 
      let path = Regex.replace ~regx:"%user%[.]*$" ~tmpl:"" config.irmin_path in
    Utils.exists path Unix.S_DIR >>= fun res ->
    err res "Invalid Irminsule path in"
  | `Mailbox ->
    Utils.exists config.inbox_path Unix.S_REG >>= fun res ->
    err res "Invalid Inbox path in " >>
    Utils.exists config.mail_path Unix.S_DIR >>= fun res ->
    err res "Invalid Mail path in "
  | `Maildir ->
    Utils.exists config.mail_path Unix.S_DIR >>= fun res ->
    err res "Invalid Maildir path in "

let store_to_string = function |`Irmin->"irmin"|`Mailbox->"mbox"|`Maildir->"maildir"

let update_config config net port ssl tls store =
  let port = (match port with |None -> config.port|Some port->port) in
  let ssl = (match ssl with |None -> config.ssl|Some ssl->ssl) in
  let starttls = (config.ssl && (match tls with |None -> config.starttls|Some tls->tls)) in
  let addr = (match net with |None -> config.addr|Some net->net) in
  let (data_store,mail_path,inbox_path) =
  begin
  match store with
  | None -> (config.data_store,config.mail_path,config.inbox_path)
  | Some (store,inbox,mail) -> (store,mail,inbox)
  end
  in
  Printf.printf "imaplet: creating imap server on %s:%d:%b:%b:%s:%s:%s\n%!"
    addr port ssl starttls (store_to_string data_store) inbox_path mail_path;
  {config with inbox_path;mail_path;addr;port;ssl;starttls;data_store}

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

let config_of_lines lines =
  let rec proc lines acc =
    match lines with 
    | hd :: tl -> 
      let acc =
      begin
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
          | "inbox_path" -> {acc with inbox_path = v}
          | "mail_path" -> {acc with mail_path = v}
          | "irmin_path" -> {acc with irmin_path = v}
          | "irmin_expand" -> {acc with irmin_expand = bval n v false}
          | "max_msg_size" -> {acc with max_msg_size = ival n v 10_000_000}
          | "lmtp_addr" -> {acc with lmtp_addr = v}
          | "lmtp_port" -> {acc with lmtp_port = ival n v 24}
          | "addr" -> {acc with addr = v}
          | "port" -> {acc with port = (ival n v 993)}
          | "ssl" -> {acc with ssl = (bval n v true)}
          | "starttls" -> {acc with starttls = (bval n v true)}
          | "data_path" -> {acc with data_path = v};
          | "pem_name" -> {acc with pem_name = v}
          | "key_name" -> {acc with key_name = v}
          | "users_path" -> {acc with users_path = v}
          | "data_store" -> {acc with data_store = (stval n v)}
          | "encrypt" -> {acc with encrypt = (bval n v true)}
          | "compress" -> {acc with encrypt = (bval n v true)}
          | _ -> Printf.printf "unknown configuration %s\n%!" n; acc
        ) else 
          acc
      end 
      in proc tl acc
    | [] -> acc
  in
  proc lines default_config

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
  let config = config_of_lines lines in
  assert ((config.data_store = `Mailbox && config.inbox_path <> "" ||
  config.data_store = `Maildir) && config.mail_path <> "" ||
  config.data_store = `Irmin);
  config
