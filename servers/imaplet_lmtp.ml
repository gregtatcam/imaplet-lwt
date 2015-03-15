(*
 * Copyright (c) 2013-2014 Gregory Tsipenyuk <gt303@cam.ac.uk>
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
open Server_config

exception InvalidCmd

let try_close chan =
  catch (fun () -> Lwt_io.close chan)
  (function _ -> return ())

let init_socket addr port =
  Printf.printf "imaplet_lmtp: creating socket %s %d\n%!" addr port;
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_of_string addr, port) in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  Lwt_unix.bind socket sockaddr;
  socket,sockaddr

let create_srv_socket () =
  let (socket,_) = init_socket srv_config.lmtp_addr srv_config.lmtp_port in
  Lwt_unix.listen socket Configuration.lmtp_backlog;
  socket

(*
 From dovecot@localhost.local  Thu Jul 17 14:53:00 2014
    for <dovecot@localhost>; Thu, 17 Jul 2014 14:53:00 +0100 (BST)
*)
let add_postmark from_ msg =
  let size = String.length msg in
  let headers = String.sub msg 0 (if size < 1024 * 5 then size else 1024 * 5) in
  if Regex.match_regex headers ~regx:"^From " = true then
    msg
  else (
    let from = "From " ^ from_ in 
    let time = 
      if Regex.match_regex headers ~regx:"^Date: \\([.]+\\)[\r\n]+" then
        try
          Dates.email_to_date_time_exn (Str.matched_group 1 headers)
        with Dates.InvalidDate -> Dates.ImapTime.now()
      else
        Dates.ImapTime.now ()
    in
    let date = Dates.postmark_date_time ~time () in
    from ^ " " ^ date ^ "\r\n" ^ msg
  )

(* sending "special" local append on unix socket
 *)
let send_to_imap from_ to_ msg =
  Printf.printf "imaplet_lmtp: sending to imap\n%!";
  let open Lwt_unix in
  let msg = add_postmark from_ msg in
  Printf.printf "imaplet_lmtp:\n%s%!" msg;
  let sockaddr = Unix.ADDR_UNIX (Filename.concat Install.data_path "sock/lmtp") in
  let socket = socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Lwt_unix.connect socket sockaddr >>= fun () ->
  let inchan = Lwt_io.of_fd ~mode:Lwt_io.input socket in
  let outchan = Lwt_io.of_fd ~mode:Lwt_io.output socket in
  let write buff =
    Lwt_io.write outchan buff >>= fun () -> Lwt_io.flush outchan
  in
  let read () =
    Lwt_io.read_line inchan
  in
  write ("a lappend " ^ to_ ^ " INBOX {" ^ (string_of_int (String.length msg)) ^ "+}\r\n") >>= fun () ->
  write msg >>= fun () ->
  read () >>= fun resp -> Printf.printf "imaplet_lmtp response: %s\n%!" resp; (* a OK APPEND completed *)
  write "a logout\r\n" >>= fun () ->
  read () >>= fun resp -> Printf.printf "imaplet_lmtp response: %s\n%!" resp; (* * BYE *)
  Lwt_unix.close socket >>= fun () ->
  try_close inchan >> try_close outchan >> return ()

(*
 From dovecot@localhost.local  Thu Jul 17 14:53:00 2014
 Return-Path: <dovecot@localhost.local>
 Delivered-To: <dovecot@localhost.local>
 Received: from gt-mba.local ([127.0.0.1])
    by gt-mba.local (Dovecot) with LMTP id KfUGDjzVx1O7OAAAEiyb6Q
    for <dovecot@localhost.local>; Thu, 17 Jul 2014 14:53:00 +0100
 Received: from [IPv6:::1] (localhost [IPv6:::1])
    by gt-mba.local (Postfix) with ESMTP id 276CCD4F1C4
    for <dovecot@localhost>; Thu, 17 Jul 2014 14:53:00 +0100 (BST)
 From: Gregory Tsipenyuk <dovecot@localhost.local>
 Content-Type: multipart/mixed; boundary="Apple-Mail=_C11681CF-9EBC-4107-A749-5B89350EC7F7"
 Subject: with image
 Message-Id: <05E1713E-577E-4CD2-A9BC-9E7DEC87E637@localhost>
 Date: Thu, 17 Jul 2014 14:52:59 +0100
 To: dovecot@localhost.local
 Mime-Version: 1.0 (Mac OS X Mail 7.3 \(1878.6\))
 X-Mailer: Apple Mail (2.1878.6)
 X-UID: 118
 Status: RO
 X-Keywords: $NotJunk NotJunk                                                
 Content-Length: 3164550

--Apple-Mail=_C11681CF-9EBC-4107-A749-5B89350EC7F7
Content-Transfer-Encoding: 7bit
Content-Type: text/plain;
charset=us-ascii

with image

--Apple-Mail=_C11681CF-9EBC-4107-A749-5B89350EC7F7
....
 *)
let process_request outchan msg buffer what =
  let open Regex in
  let (from_,to_,state) = what in
  let write buff =
    Lwt_io.write outchan buff >>= fun () -> Lwt_io.flush outchan
  in
  match state with
  | `Start ->
    if match_regex msg ~regx:"^LHLO" = true then
      write "250\r\n" >>= fun () -> return (from_,to_,`WaitingData)
    else
      raise InvalidCmd
  | `WaitingData ->
    let from_ =
      if match_regex msg ~regx:"^MAIL FROM:<\\([^>]*\\)" then
        Str.matched_group 1 msg
      else
        from_
    in
    let to_ =
      if match_regex msg ~regx:"^RCPT TO:<\\([^@]*\\)" then
        Str.matched_group 1 msg
      else
        to_
    in
    if match_regex msg ~regx:"^DATA" = true then (
      write "354 OK\r\n" >>= fun () -> return (from_,to_,`Data)
    ) else (
      write "250 OK\r\n" >>= fun () -> return (from_,to_,`WaitingData)
    )
  | `Data ->
    if match_regex msg ~regx:".\r\n$" = true then (
      Buffer.add_substring buffer msg 0 ((String.length msg - 3));
      try_lwt
        send_to_imap from_ to_ (Buffer.contents buffer) >>= fun () ->
        write "250 OK\r\n" >>= fun () -> return (from_,to_,`Quit)
      with | _ ->
        Printf.printf "imaplet_lmtp: failed to send to imap\n%!";
        write "451\r\n" >>= fun () -> return (from_,to_,`Done)
    ) else (
      Buffer.add_string buffer msg;
      return (from_,to_,`Data)
    )
  | `Quit ->
    if match_regex msg ~regx:"^QUIT\r\n" = true then (
      write "221 OK\r\n" >>= fun () -> return (from_,to_,`Done)
    ) else
      raise InvalidCmd
  | `Done -> raise InvalidCmd

let rec requests inchan outchan buffer what =
  try
  catch (fun () -> 
    Lwt_io.read ~count:10240 inchan >>= fun msg -> Printf.printf "imaplet_lmtp: requests\n%!";
    process_request outchan msg buffer what >>= fun what ->
      let (_,_,state) = what in
      match state with
      | `Done -> return ()
      | _ -> requests inchan outchan buffer what
  )
  (fun ex -> Printf.printf "imaplet_lmtp: connection closed %s\n%!"
  (Printexc.get_backtrace()); return ())
  with ex -> Printf.printf "imaplet_lmtp: exception %s\n" (Printexc.to_string ex); return ()
 
let process socket =
  Printf.printf "imaplet_lmtp: processing socket\n%!";
  let rec _process () =
    Lwt_unix.accept socket >>=
      (fun (socket_cli, _) ->
        Printf.printf "imaplet_lmtp: accepted socket\n%!";
        let inchan = Lwt_io.of_fd ~mode:Lwt_io.input socket_cli in
        let outchan = Lwt_io.of_fd ~mode:Lwt_io.output socket_cli in
        async (fun () -> 
          Lwt_io.write outchan "220 LMTP server ready\r\n" >>= fun () -> 
            requests inchan outchan (Buffer.create 0) ("","",`Start) >>= fun () ->
              Lwt_unix.close socket_cli >>= fun () ->
              try_close inchan >> try_close outchan >> return ()
            );
        _process ()
      )
  in
  _process ()
 
let _ =
  Printf.printf "imaplet_lmtp: started\n%!";
  let socket = create_srv_socket() in 
  Lwt_main.run (
    process socket
  )
