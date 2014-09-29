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
open Irmin_core
open Irmin_storage

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
  Printf.printf "usage: imaplet_irmin_build -u [user] -i [inbox location] -m [mailboxes location]"

let commands f =
  let open Core.Std in
  try 
    let user,inbx,mbx = args 1 None None None in
    if inbx = None || mbx = None || user = None then
      usage ()
    else
      try 
        f (Option.value_exn user) (Option.value_exn inbx) (Option.value_exn mbx)
      with ex -> Printf.printf "%s\n%!" (Exn.to_string ex)
  with _ -> usage ()

let rec listdir path mailbox f =
  Printf.printf "listing %s\n%!" path;
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
  IrminStorage.create user mailbox >>= fun ist ->
  IrminStorage.create_mailbox ist >>
  return ist

let populate_mailbox ist path mailbox =
  let open Core.Std in
  let open Regex in
  let open Storage_meta in
  let open Email_message in
  let open Email_message.Mailbox in
  let (strm,strm_push) = Lwt_stream.create () in
  async ( fun () ->
    let wseq = With_seq.t_of_file path in
    With_seq.iter_string wseq (fun message ->
      let substr = 
        if String.length message > 1024 then
          String.slice message 0 1024
        else
          message
      in
      if match_regex substr ~regx:"^From[ ]+MAILER_DAEMON" then (
        ()
      ) else ( 
        let (postmark,email) =
        if match_regex substr ~regx:"^\\(From [^\r\n]+\\)\r?\n" then ( 
          let postmark = Str.matched_group 1 message in
          let email = Str.last_chars message ((String.length message) - 
            (String.length (Str.matched_string message))) in
          (postmark,email)
        ) else (
          let from =
          if match_regex substr ~regx:"^From: \\([^<]+\\)<\\([^>]+\\)" then
            Str.matched_group 2 substr
          else
            "From daemon@localhost.local"
          in
          let postmark = replace ~regx:"DATE" 
              ~tmpl:(Dates.postmark_date_time()) "From FROM DATE" in
          let postmark = replace ~regx:"FROM" 
              ~tmpl:from postmark in
          (postmark,message)
        )
        in
        let message = {Message.postmark=Mailbox.Postmark.of_string
          postmark;email=Email.of_string email} in
        strm_push (Some message)
      )
    );
    strm_push None;
    return ()
  );
  Lwt_stream.iter_s (fun message ->
    let metadata = empty_mailbox_message_metadata() in
    let metadata = {metadata with flags=[Imaplet_types.Flags_Recent]} in
    IrminStorage.append ist message metadata
  ) strm

let create_inbox user inbx =
  Printf.printf "creating mailbox: INBOX\n%!";
  create_mailbox user "INBOX" >>= fun ist ->
  populate_mailbox ist inbx "INBOX" >>
  IrminStorage.commit ist

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
            Printf.printf "creating mailbox folder: %s\n%!" mailbox;
            create_mailbox user (mailbox ^ "/") >>= fun ist ->
            IrminStorage.commit ist
          ) else (
            Printf.printf "creating mailbox: %s\n%!" mailbox;
            create_mailbox user mailbox >>= fun ist ->
            populate_mailbox ist path mailbox >>
            IrminStorage.commit ist
          )
        )
      )
      (fun ex -> Printf.printf "exception %s %s\n%!" (Exn.to_string ex) (Exn.backtrace());return())
    )
  )
