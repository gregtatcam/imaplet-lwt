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
open BatLog
open Irmin_core
open Irmin_storage

Easy.level := `debug;;
Easy.output := BatIO.stderr;;

exception InvalidCommand

let rec args i user inbx mbx =
  let open Core.Std in
  if Array.length Sys.argv <> 7 || i >= 7 then
    user,inbx,mbx
  else
    match Sys.argv.(i) with 
    | "-i" -> args (i+2) user (Some Sys.argv.(i+1)) mbx
    | "-m" -> args (i+2) user inbx (Some Sys.argv.(i+1))
    | "-u" -> args (i+2) (Some Sys.argv.(i+1)) inbx mbx
    | _ -> raise InvalidCommand

let usage () =
  Easy.logf `debug "usage: imaplet_irmin_build -u [user] -i [inbox location] -m [mailboxes location]"

let commands f =
  let open Core.Std in
  try 
    let user,inbx,mbx = args 1 None None None in
    if inbx = None || mbx = None || user = None then
      usage ()
    else
      try 
        f (Option.value_exn user) (Option.value_exn inbx) (Option.value_exn mbx)
      with ex -> Easy.logf `debug "%s\n%!" (Exn.to_string ex)
  with _ -> usage ()

let rec listdir path mailbox f =
  Easy.logf `debug "listing %s\n%!" path;
  let strm = Lwt_unix.files_of_directory path in
  Lwt_stream.iter_s (fun i -> 
    if i = "." || i = ".." || i = ".imap" || i = ".imaplet" || i = ".subscriptions" then
      return ()
    else (
      let path = Filename.concat path i in
      let mailbox = Filename.concat mailbox i in
      Lwt_unix.stat path >>= fun stat ->
      if stat.Lwt_unix.st_kind = Lwt_unix.S_DIR then
        f true path mailbox >>
        listdir path mailbox f 
      else
        f false path mailbox
    )
  ) strm 

let create_mailbox user mailbox =
  let ist = IrminStorage.create user in
  IrminStorage.create_mailbox ist mailbox >>
  return ist

let populate_mailbox ist path mailbox =
  let open Email_message in
  let open Email_message.Mailbox in
  let wseq = With_seq.t_of_file path in
  With_seq.iter_string wseq (fun message ->
    if Regex.match_regex message ~regx:"From[ ]+MAILER_DAEMON" then
      ()
    else if Regex.match_regex message ~regx:"\\(From [^\r\n]+\\)\r?\n" then ( 
      let postmark = Str.matched_group 1 message in
      let email = Str.last_chars message ((String.length message) - 
        (String.length (Str.matched_string message))) in
      on_success (IrminStorage.append ist mailbox 
        {Message.postmark=Mailbox.Postmark.of_string postmark;email=Email.of_string email} 
        (Storage_meta.empty_mailbox_message_metadata())) (fun () -> ()) 
    ) else 
      ()
  )

let create_inbox user inbx =
  create_mailbox user "INBOX" >>= fun ist ->
  populate_mailbox ist inbx "INBOX";
  return ()

let () =
  let open Core.Std in
  commands (fun user inbx mbx ->
    Lwt_main.run (
      catch ( fun () ->
        let open Core.Std in
        let ac = UserAccount.create user in
        UserAccount.delete_account ac >>= fun _ ->
        UserAccount.create_account ac >>= fun _ ->
        create_inbox user inbx >>
        listdir mbx "/" (fun is_dir path mailbox ->
          if is_dir then (
            Easy.logf `debug "creating mailbox folder: %s\n%!" mailbox;
            create_mailbox user (mailbox ^ "/") >>= fun _ -> return ()
          ) else (
            Easy.logf `debug "creating mailbox: %s\n%!" mailbox;
            create_mailbox user mailbox >>= fun ist ->
            populate_mailbox ist path mailbox;
            return ()
          )
        )
      )
      (fun ex -> Easy.logf `debug "exception %s %s\n%!" (Exn.to_string ex) (Exn.backtrace());return())
    )
  )
