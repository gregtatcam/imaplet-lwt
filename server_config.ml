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
  inbox_path : string; (* inbox location, default /var/mail *)
  mail_path : string; (* mailboxes location, default /Users/user-name/mail *)
  irmin_path : string; (* irminsule location, default / *)
  max_msg_size : int;
  imap_name : string; (* greeting name, default imaplet *)
  imap_addr : string; (* imap address, default 127.0.0.1 *)
  imap_port : int; (* local imap port, default 6002 *)
  irmin_addr : string; (* irmin server addres, default 127.0.0.1 *)
  irmin_port : int; (* irmin server port, default 20001 *)
  lmtp_addr : string; (* lmtp server address, default 127.0.0.1 *)
  lmtp_port : int; (* lmtp server port, default 24 *)
  addr : string ref; (* server port, default 127.0.0.1 *)
  port : int ref; (* server port, default 993 *)
  ssl : bool ref; (* ssl enabled, default true *)
  starttls : bool ref; (* starttls enabled, default true *)
  cert_path : string; (* pam/key path, default datadir/imaplet *)
  pem_name : string; (* pem file name, default server.pem *)
  key_name : string; (* private key file name, default server.key *)
  users_path : string; (* users file path, default datadir/imaplet *)
}

let srv_config = 
  let open Batteries in
  let lines =
    let en = BatFile.lines_of Install.config_path in
    BatEnum.fold (fun acc e -> e :: acc) [] en
  in
  BatLog.Easy.logf `debug "##### loading configuration file #####\n%!";
  let config = {
    rebuild_irmin = false;
    inbox_path = "/var/mail";
    mail_path = "/Users/@/mail";
    irmin_path = "/tmp/irmin/test";
    max_msg_size = 0;
    imap_name = "imaplet";
    imap_addr = "127.0.0.1";
    imap_port = 6000;
    irmin_addr = "127.0.0.1";
    irmin_port = 6001;
    lmtp_addr = "127.0.0.1";
    lmtp_port = 24;
    addr = ref "127.0.0.1";
    port = ref 993;
    ssl = ref true;
    starttls = ref true;
    cert_path = Install.cert_path;
    pem_name = "server.pem";
    key_name = "server.key";
    users_path = Install.users_path;
  } in
  let rec proc lines acc =
    match lines with 
    | hd :: tl -> 
    let acc =
    (
    let matched = try 
        Regex.match_regex ~regx:"^\\([^# ][^ ]+\\) \\([^ ]+$\\)" hd
      with _ -> false
    in
    if matched then (
      let n = Str.matched_group 1 hd in
      let v = Str.matched_group 2 hd in
      let log n v = BatLog.Easy.logf `debug "%s: invalid value %s\n%!" n v in
      let ival n v default = try int_of_string v with _ -> log n v; default in
      let bval n v default = try bool_of_string v with _ -> log n v; default in
      match n with 
      | "imap_name" -> {acc with imap_name = v }
      | "rebuild_irmin" -> {acc with rebuild_irmin = bval n v false}
      | "inbox_path" -> {acc with inbox_path = v}
      | "mail_path" -> {acc with mail_path = v}
      | "irmin_path" -> {acc with irmin_path = v}
      | "max_msg_size" -> {acc with max_msg_size = ival n v 10_000_000}
      | "imap_addr" -> {acc with imap_addr = v}
      | "imap_port" -> {acc with imap_port = ival n v 6000}
      | "irmin_addr" -> {acc with irmin_addr = v}
      | "irmin_port" -> {acc with irmin_port = ival n v 6001}
      | "lmtp_addr" -> {acc with lmtp_addr = v}
      | "lmtp_port" -> {acc with lmtp_port = ival n v 24}
      | "addr" -> {acc with addr = ref v}
      | "port" -> {acc with port = ref (ival n v 993)}
      | "ssl" -> {acc with ssl = ref (bval n v true)}
      | "starttls" -> {acc with starttls = ref (bval n v true)}
      | "cert_path" -> {acc with cert_path = v};
      | "pem_name" -> {acc with pem_name = v}
      | "key_name" -> {acc with key_name = v}
      | "users_path" -> {acc with users_path = v}
      | _ -> BatLog.Easy.logf `debug "unknown configuration %s\n%!" n; acc
    ) else 
      acc
    ) in proc tl acc
    | [] -> acc
  in
  proc lines config
