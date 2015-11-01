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

type store_type = [`Irmin|`Mailbox|`Maildir|`Workdir|`Gitl]

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
  data_store : store_type; (* type of storage,
  irmin/maildir/workdir/gitl supported *)
  encrypt : bool; (* encrypt messages, default true *)
  compress : bool; (* compress messages, but not attachments, default true *)
  compress_attach : bool; (* compress attachments, default false *)
  compress_repo : int option; (* repository compress, gitl only so far, should use
  level 0 for irmin, default None *)
  user_cert_path : string; (* user's certificate/key location *)
  log : string; (* log location, default /var/log *)
  log_level:[`Error|`Info1|`Info2|`Info3|`Debug|`None]; (* log level, default error *)
  idle_interval: float; (* wait (sec) between idle 'still here' notifications, default 120 sec *)
  smtp_idle_max: float; (* smtp idle time-out, default 300 sec *)
  auth_required: bool; (* require user authentication, priv key encrypted with password, default true *)
  stun_header: bool; (* include STUN mapped address in the header - can be used
  for direct communication with a peer, default false *)
  domain: string; (* email domain, by default host name *)
  maildir_parse: bool; (* parse message into MIME parts when in maildir storage format, default true *)
  single_store: bool; (* single-store attachments in irmin and workdir format, default true *)
  hybrid: bool; (* hybrid of irmin and workdir store (store should be set to irmin, default false *)
  (* list of name servers/domains for DNS resolution, default None *)
  resolve: [`File of string|`NS of ((string*string) list) * (string list)] option;
  relayfrom: string option; (* file defining 'from' users allowed to relay, default None *)
  relay_authreq: bool; (* require authenticated user to relay, default false *)
  replicate: bool; (* replicate client with master, default true (master must be set too) *)
  master: string option; (* master address, used for replication, default None *)
  master_repo: string option; (* master repo location, default None - same as irmin_path *)
  replicate_interval: float; (* frequency of replication polling in sec, default
  30sec, master must be set *)
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
  compress_attach = false;
  compress_repo = None;
  user_cert_path = "/var/mail/accounts/%user%/cert";
  log = "/var/log";
  log_level = `Error;
  idle_interval = 120.;
  smtp_idle_max = 300.;
  auth_required = true;
  stun_header = false;
  domain = Unix.gethostname ();
  maildir_parse = true;
  single_store = true;
  hybrid = false;
  resolve = None;
  relayfrom = None;
  relay_authreq = false;
  replicate = true;
  master = None;
  master_repo = None;
  replicate_interval = 30.;
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
  | `Gitl -> 
    let path = Regex.replace ~regx:"%user%.*$" ~tmpl:"" config.irmin_path in
    Utils.exists path Unix.S_DIR >>= fun res ->
    err res "Invalid Gitl path in"
  | `Mailbox ->
    let path = Regex.replace ~regx:"%user%.*$" ~tmpl:"" config.inbox_path in
    Utils.exists path Unix.S_REG >>= fun res ->
    err res "Invalid Inbox path in " >>
    let path = Regex.replace ~regx:"%user%.*$" ~tmpl:"" config.mail_path in
    Utils.exists path Unix.S_DIR >>= fun res ->
    err res "Invalid Mail path in "
  | `Maildir ->
    let path = Regex.replace ~regx:"%user%.*$" ~tmpl:"" config.mail_path in
    Utils.exists path Unix.S_DIR >>= fun res ->
    err res "Invalid Maildir path in "
  | `Workdir ->
    let path = Regex.replace ~regx:"%user%.*$" ~tmpl:"" config.irmin_path in
    Utils.exists path Unix.S_DIR >>= fun res ->
    err res "Invalid Irminsule path in"
  end >>= function
  | `Error err -> return (`Error err)
  | `Ok ->
    err (config.auth_required = false || config.auth_required && config.smtp_starttls = true &&
      (config.ssl = true || config.starttls = true))
      "ssl/starttls have to be enabled when auth_required is enabled"

let update_config config net port ssl tls store encrypt compress compress_attach =
  let port = (match port with |None -> config.port|Some port->port) in
  let ssl = (match ssl with |None -> config.ssl|Some ssl->ssl) in
  let starttls = (config.ssl && (match tls with |None -> config.starttls|Some tls->tls)) in
  let encrypt = (match encrypt with |None -> config.encrypt|Some encrypt->encrypt) in
  let compress = (match compress with |None -> config.compress|Some compress->compress) in
  let compress_attach = (match compress_attach with |None ->
    config.compress_attach|Some compress_attach->compress_attach) in
  let addr = (match net with |None -> config.addr|Some net->net) in
  let (data_store,mail_path,inbox_path) =
  begin
  match store with
  | None -> (config.data_store,config.mail_path,config.inbox_path)
  | Some (store,inbox,mail) -> (store,mail,inbox)
  end
  in
  {config with
  inbox_path;mail_path;addr;port;ssl;starttls;data_store;encrypt;compress;compress_attach}

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

let resolve_of_string log v =
  let regxns = 
    "^ns:\\([^,;:]+\\(:[0-9]+\\)?\\(,[^,;:]+\\(:[0-9]+\\)?\\)*\\)\\(;\\([^,;]+\\(,[^,;]+\\)*\\)\\)?$" in
  let regxfile = "^file:\\(.+\\)$" in
  if Regex.match_regex ~regx:regxns v then (
    let iplist = Str.split (Str.regexp ",") (Str.matched_group 1 v) in
    let iplist = List.map (fun s -> 
      if Regex.match_regex ~regx:"^\\([^:]+\\):\\(.+\\)$" s then 
        Str.matched_group 1 s, Str.matched_group 2 s
      else
        s,"53"
    ) iplist in
    let domainlist = try Str.split (Str.regexp ",") (Str.matched_group 6 v) with _ -> [] in
    Some (`NS (iplist,domainlist))
  ) else if Regex.match_regex ~regx:regxfile v then(
    Some (`File (Str.matched_group 1 v))
  ) else (
    log v;
    None
  )

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
            |"irmin"->`Irmin|"gitl"->`Gitl|"mbox"->`Mailbox|"maildir"->`Maildir|"workdir"->`Workdir
            |_->log n v; `Irmin in
          match n with 
          | "imap_name" -> {acc with imap_name = v }
          | "rebuild_irmin" -> {acc with rebuild_irmin = bval n v false}
          | "inbox_path" -> {acc with inbox_path = v}
          | "mail_path" -> {acc with mail_path = v}
          | "irmin_path" -> {acc with irmin_path = v;master_repo=if
            acc.master_repo <> None then acc.master_repo else Some v}
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
          | "compress" -> {acc with compress = (bval n v true)}
          | "compress_attach" -> {acc with compress_attach = (bval n v true)}
          | "compress_repo" -> 
            let compress_repo = if v = "-" then None else Some (int_of_string v) in
            {acc with compress_repo}
          | "auth_required" -> {acc with auth_required = (bval n v true)}
          | "user_cert_path" -> {acc with user_cert_path = v}
          | "idle_interval" -> {acc with idle_interval = fval n v 120.}
          | "smtp_idle_max" -> {acc with smtp_idle_max = fval n v 300.}
          | "log" -> {acc with log = v}
          | "stun_header" -> {acc with stun_header = (bval n v false)}
          | "log_level" -> {acc with log_level =
            match v with
            | "error" -> `Error
            | "info1" -> `Info1
            | "info2" -> `Info2
            | "info3" -> `Info3
            | "debug" -> `Debug
            | "none" -> `None
            | _ -> log n v;`Error}
          | "domain" -> {acc with domain = v}
          | "maildir_parse" -> {acc with maildir_parse = (bval n v true)}
          | "single_store" -> {acc with single_store = (bval n v true)}
          | "hybrid" -> {acc with hybrid = (bval n v true)}
          | "resolve" -> {acc with resolve = resolve_of_string (log n) v}
          | "relayfrom" -> {acc with relayfrom = Some v}
          | "relay_authreq" -> {acc with relay_authreq = bval n v false}
          | "replicate" -> {acc with replicate = bval n v true}
          | "master" -> {acc with master = Some v}
          | "master_repo" -> {acc with master_repo = Some v}
          | "replicate_interval" -> {acc with replicate_interval = fval n v 30.}
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
  config.data_store = `Irmin || config.data_store = `Gitl || config.data_store = `Workdir);
  config
