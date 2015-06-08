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
open Imaplet
open Commands
open Commands.Server_config
open Commands.Dates

exception InvalidCommand

let _ = Log_.set_log "imaplet.log"

let rec args i net port ssl tls store encrypt compress =
  let bval = function
  | "true" -> true
  | "false" -> false
  | _ -> raise InvalidCommand
  in
  let sval str = 
    if str = "irmin" then
      (`Irmin,"","")
    else if str = "workdir" then
      (`GitWorkdir,"","")
    else if Regex.match_regex ~regx:"^mbox:\\([^,]+\\),\\([.]+\\)$" str then
      (`Mailbox,(Str.matched_group 1 str),(Str.matched_group 2 str))
    else if Regex.match_regex ~regx:"^maildir:\\([^,]+\\)$" str then
      (`Maildir,"",(Str.matched_group 1 str))
    else
      raise InvalidCommand
  in
  if i >= Array.length Sys.argv then
    net,port,ssl,tls,store,encrypt,compress
  else
    match Sys.argv.(i) with 
    | "-net" -> args (i+2) (Some Sys.argv.(i+1)) port ssl tls store encrypt compress
    | "-port" -> args (i+2) net (Some (int_of_string Sys.argv.(i+1))) ssl tls store encrypt compress
    | "-ssl" -> args (i+2) net port (Some (bval Sys.argv.(i+1))) tls store encrypt compress
    | "-starttls" -> args (i+2) net port ssl (Some (bval Sys.argv.(i+1))) store encrypt compress
    | "-store" -> args (i+2) net port ssl tls (Some (sval Sys.argv.(i+1))) encrypt compress
    | "-encrypt" -> args (i+2) net port ssl tls store (Some (bval Sys.argv.(i+1))) compress
    | "-compress" -> args (i+2) net port ssl tls store encrypt (Some (bval Sys.argv.(i+1))) 
    | _ -> raise InvalidCommand

let usage () =
  Log_.log `Error "usage: imaplet -net [interface] -port [port] -ssl [true|false]
  -starttls [true|false] -store[irmin|workdir|mbox:inboxpath,mailboxpath|maildir:maildirpath
  -encrypt [true|false] -compress [true|store]\n"

let commands f =
  try 
    let net,port,ssl,tls,store,encrypt,compress = args 1 None None None None None None None in
      try 
        f net port ssl tls store encrypt compress
      with ex -> Log_.log `Error (Printf.sprintf "%s\n" (Printexc.to_string ex))
  with _ -> usage ()

let log config =
  Log_.log `Info1 
    (Printf.sprintf 
      "imaplet: creating imap server %s: on addr/port %s:%d ssl/starttls %b:%b 
      encrypt/compress %b:%b\n"
      (ImapTime.to_string (ImapTime.now()))
      config.addr config.port config.ssl config.starttls config.encrypt config.compress);
  match config.data_store with
  | `Irmin -> Log_.log `Info1 (Printf.sprintf "storage: irmin:%s\n" config.irmin_path)
  | `GitWorkdir -> Log_.log `Info1 (Printf.sprintf "storage: workdir:%s\n" config.irmin_path)
  | `Maildir -> Log_.log `Info1 (Printf.sprintf "storage: maildir:%s\n" config.mail_path)
  | `Mailbox -> Log_.log `Info1 (Printf.sprintf "storage: mailbox:%s:%s\n" config.inbox_path config.mail_path)

(**
 * start the server
**)
let () = 
  try
    commands 
      (fun net port ssl tls store encrypt compress ->
        Lwt_main.run (catch(fun() ->
            let config = update_config srv_config net port ssl tls store encrypt compress in
            log config;
            Server.create config >>= function
            | `Ok -> return ()
            | `Error e -> Log_.log `Error e; return ()
          )
          (fun ex -> Log_.log `Error (Printf.sprintf "imaplet: fatal exception: %s %s"
            (Printexc.to_string ex) (Printexc.get_backtrace())); return()
          )
        )
      )
  with Exit -> 
    Log_.log `Error (Printf.sprintf "imaplet: terminated: %s" (Printexc.get_backtrace()))
