(*
 * Copyright (c) 2013-2015 Gregory Tsipenyuk <gregtsip@cam.ac.uk>
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
open Maildir_storage
open Server_config

exception InvalidCommand

(* -u : user
 * -m : maildir-path
 *)
let rec args i user mailbox =
  if i >= Array.length Sys.argv then
    user,mailbox
  else
    match Sys.argv.(i) with 
    | "-m" -> args (i+2) user (Some Sys.argv.(i+1))
    | "-u" -> args (i+2) (Some Sys.argv.(i+1)) mailbox
    | _ -> raise InvalidCommand

(* support existing maildir only *)
let usage () =
  Printf.printf "usage: imaplet_init_maildir -u [user] -m [maildir-path]\n%!"

let commands f =
  try 
    let user,mbx = args 1 None None in
    if user = None || mbx = None then
      usage ()
    else
      try 
        f (Utils.option_value_exn user) (Utils.option_value_exn mbx)
      with ex -> Printf.printf "%s\n%!" (Printexc.to_string ex)
  with _ -> usage ()

let () =
  commands (fun user mbx ->
    Printexc.record_backtrace true;
    Lwt_main.run (
      catch ( fun () ->
        Ssl_.get_user_keys ~user srv_config >>= fun keys ->
        MaildirStorage.create srv_config user "inbox" keys >>= fun t ->
        MaildirStorage.create_mailbox t
      )
      (fun ex -> Printf.printf "exception: %s %s\n%!" (Printexc.to_string ex)
      (Printexc.get_backtrace());return())
    )
  )
