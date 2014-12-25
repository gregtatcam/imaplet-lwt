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

exception InvalidCommand

let store_to_string = function |`Irmin->"irmin"|`Mailbox->"mbox"|`Maildir->"maildir"

let update_config net port ssl tls store =
  let open Server_config in
  srv_config.port := (match port with |None -> srv_config.!port|Some port->port);
  srv_config.ssl := (match ssl with |None -> srv_config.!ssl|Some ssl->ssl);
  srv_config.starttls := (srv_config.!ssl && (match tls with |None -> srv_config.!starttls|Some tls->tls));
  srv_config.addr := (match net with |None -> srv_config.!addr|Some net->net);
  begin
  match store with
  | None -> ()
  | Some (store,inbox,mail) -> srv_config.data_store := store;
    srv_config.mail_path := mail; srv_config.inbox_path := inbox;
  end;
  Printf.printf "imaplet: creating imap server on %s:%d:%b:%b:%s:%s:%s\n%!"
    srv_config.!addr srv_config.!port srv_config.!ssl srv_config.!starttls 
    (store_to_string srv_config.!data_store) srv_config.!inbox_path
    srv_config.!mail_path

let rec args i net port ssl tls store =
  let bval = function
  | "true" -> true
  | "false" -> false
  | _ -> raise InvalidCommand
  in
  let sval str = 
    if str = "irmin" then
      (`Irmin,"","")
    else if Regex.match_regex ~regx:"^mbox:\\([^,]+\\),\\([.]+\\)$" str then
      (`Mailbox,(Str.matched_group 1 str),(Str.matched_group 2 str))
    else if Regex.match_regex ~regx:"^maildir:\\([^,]+\\)$" str then
      (`Maildir,"",(Str.matched_group 1 str))
    else
      raise InvalidCommand
  in
  if i >= Array.length Sys.argv then
    net,port,ssl,tls,store
  else
    match Sys.argv.(i) with 
    | "-net" -> args (i+2) (Some Sys.argv.(i+1)) port ssl tls store
    | "-port" -> args (i+2) net (Some (int_of_string Sys.argv.(i+1))) ssl tls store
    | "-ssl" -> args (i+2) net port (Some (bval Sys.argv.(i+1))) tls store
    | "-tls" -> args (i+2) net port ssl (Some (bval Sys.argv.(i+1))) store
    | "-store" -> args (i+2) net port ssl tls (Some (sval Sys.argv.(i+1))) 
    | _ -> raise InvalidCommand

let usage () =
  Printf.printf "usage: imaplet -net [interface] -port [port] -ssl [true|false]
  -tls [true|false] -store[irmin;mbox:inboxpath,mailboxpath;maildir:maildirpath\n%!"

let commands f =
  try 
    let net,port,ssl,tls,store = args 1 None None None None None in
      try 
        f net port ssl tls store
      with ex -> Printf.printf "%s\n%!" (Printexc.to_string ex)
  with _ -> usage ()


let validate_config () =
  let open Server_config in
  let err res msg =
    begin
    if res = false then (
      Printf.printf "%s %s\n%!" msg Install.config_path;
      assert (false)
    ) else
      return ()
    end
  in
  match srv_config.!data_store with
  | `Irmin -> 
    Utils.exists srv_config.irmin_path Unix.S_DIR >>= fun res ->
    err res "Invalid Irminsule path in"
  | `Mailbox ->
    Utils.exists srv_config.!inbox_path Unix.S_REG >>= fun res ->
    err res "Invalid Inbox path in " >>
    Utils.exists srv_config.!mail_path Unix.S_DIR >>= fun res ->
    err res "Invalid Mail path in "
  | `Maildir ->
    Utils.exists srv_config.!mail_path Unix.S_DIR >>= fun res ->
    err res "Invalid Maildir path in "

(**
 * start the server
**)
let () = 
  try
    commands 
      (fun net port ssl tls store ->
        Lwt_main.run (catch(fun() ->
            update_config net port ssl tls store;
            validate_config () >>
            Server.create ()
          )
          (fun ex -> Printf.printf "imaplet: fatal exception: %s %s"
            (Printexc.to_string ex) (Printexc.get_backtrace()); return()
          )
        )
      )
  with Exit -> 
    Printf.printf "imaplet: terminated: %s" (Printexc.get_backtrace())
