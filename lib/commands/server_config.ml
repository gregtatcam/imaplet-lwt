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
  smtp_addr : string; (* smtp server address, default 127.0.0.1 *)
  smtp_port : int list; (* smtp server ports to listen on, default [25;587] *)
  smtp_ssl : bool; (* ssl enabled for smtp, default false *)
  smtp_starttls : bool; (* starttls enabled for smtp, default false *)
  addr : string; (* server port, default 127.0.0.1 *)
  port : int; (* server port, default 993 *)
  ssl : bool; (* ssl enabled, default true *)
  starttls : bool; (* starttls enabled, default true *)
  data_path : string; (* pam/key path, default datadir/imaplet *)
  pem_name : string; (* pem file name, default server.pem *)
  key_name : string; (* private key file name, default server.key *)
  pub_name : string; (* public key file name, default server.pub *)
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

let default_config = {
  rebuild_irmin = false;
  inbox_path = "";(*"/var/mail";*)
  mail_path = "";(*"/Users/@/mail";*)
  irmin_path = "/var/mail/accounts/%user%/repo";
  irmin_expand = false;
  max_msg_size = 0;
  imap_name = "imaplet";
  smtp_addr = "127.0.0.1";
  smtp_port = [25;587];
  smtp_ssl = false;
  smtp_starttls = true;
  addr = "127.0.0.1";
  port = 993;
  ssl = true;
  starttls = true;
  data_path = Install.data_path;
  pem_name = "server.pem";
  key_name = "server.key";
  pub_name = "server.pub";
  users_path = Install.users_path;
  data_store = `Irmin;
  encrypt = true;
  compress = true;
  user_cert_path = "/var/mail/accounts/%user%/cert";
  log = "/var/log";
  log_level = `Error;
  idle_interval = 120.;
  smtp_idle_max = 300.;
  auth_required = true;
}

let validate_config config =
  let err res msg =
    begin
    if res = false then (
      return (`Error (Printf.sprintf "%s %s\n" msg Install.config_path))
    ) else
      return `Ok
    end
  in
  begin
  match config.data_store with
  | `Irmin -> 
      let path = Regex.replace ~regx:"%user%.*$" ~tmpl:"" config.irmin_path in
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
  end >>= function
  | `Error err -> return (`Error err)
  | `Ok ->
    err (config.auth_required = false || config.auth_required && config.smtp_starttls = true &&
      (config.ssl = true || config.starttls = true))
      "ssl/starttls have to be enabled when auth_required is enabled"

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
          let log n v = Printf.fprintf stderr "%s: invalid value %s\n%!" n v in
          let ival n v default = try int_of_string v with _ -> log n v; default in
          let bval n v default = try bool_of_string v with _ -> log n v; default in
          let fval n v default = try float_of_string v with _ -> log n v; default in
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
          | "smtp_addr" -> {acc with smtp_addr = v}
          | "smtp_port" -> 
            let smtp_port = List.fold_left (fun acc p -> 
              (int_of_string p) :: acc
            ) [] (Str.split (Str.regexp ",") v)
            in
            { acc with smtp_port}
          | "smtp_ssl" -> {acc with smtp_ssl = bval n v false}
          | "smtp_starttls" -> {acc with smtp_starttls = bval n v true}
          | "addr" -> {acc with addr = v}
          | "port" -> {acc with port = (ival n v 993)}
          | "ssl" -> {acc with ssl = (bval n v true)}
          | "starttls" -> {acc with starttls = (bval n v true)}
          | "data_path" -> {acc with data_path = v};
          | "pem_name" -> {acc with pem_name = v}
          | "key_name" -> {acc with key_name = v}
          | "pub_name" -> {acc with pub_name = v}
          | "users_path" -> {acc with users_path = v}
          | "data_store" -> {acc with data_store = (stval n v)}
          | "encrypt" -> {acc with encrypt = (bval n v true)}
          | "compress" -> {acc with encrypt = (bval n v true)}
          | "auth_required" -> {acc with auth_required = (bval n v true)}
          | "user_cert_path" -> {acc with user_cert_path = v}
          | "idle_interval" -> {acc with idle_interval = fval n v 120.}
          | "smtp_idle_max" -> {acc with smtp_idle_max = fval n v 300.}
          | "log" -> {acc with log = v}
          | "log_level" -> {acc with log_level =
            match v with
            | "error" -> `Error
            | "info1" -> `Info1
            | "info2" -> `Info2
            | "info3" -> `Info3
            | "debug" -> `Debug
            | "none" -> `None
            | _ -> log n v;`Error}
          | _ -> Printf.fprintf stderr "unknown configuration %s\n%!" n; acc
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
