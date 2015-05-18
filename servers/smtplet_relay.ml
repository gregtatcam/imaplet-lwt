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
open Socket_utils
open Smtplet_context

let write_relay w msg =
  Log_.log `Info3 (Printf.sprintf "<-- relayed: %s\n" msg);
  Lwt_io.write w (msg ^ "\r\n") 

let read_relay r =
  Lwt.pick [
    (Lwt_unix.sleep 30. >> return None);
    Lwt_io.read_line_opt r;
  ] >>= function
  | Some str -> Log_.log `Info3 (Printf.sprintf "--> relayed: %s\n" str); return (Some str)
  | None -> return None

let read_relay_rc r rc =
  read_relay r >>= function
  | Some str -> 
    if Regex.match_regex ~regx:rc str then
      return `Ok
    else if Regex.match_regex ~regx:"^55[0-9]" str then
      return (`PermanentFailure str)
    else
      return `Failure
  | None -> return `Failure

let send_data r w context =
  write_relay w "DATA" >>
  read_relay_rc r "^250\\|354" >>= fun res ->
  if res = `Ok then (
    let lwt_buff = Lwt_bytes.of_bytes (Buffer.contents context.buff) in
    let dc = Lwt_io.of_bytes ~mode:Lwt_io.input lwt_buff in
    let rec send () =
      Lwt_io.read_line_opt dc >>= function
      | Some str -> write_relay w str >> send ()
      | None -> write_relay w "." >>
        read_relay_rc r "^250" >>= fun res ->
        write_relay w "QUIT" >>
        return res
    in
    send ()
  ) else (
    return res
  )

let send_rcptto r w context =
  let (user,domain,_) = List.hd context.rcpt in
  write_relay w (Printf.sprintf "RCPT TO: <%s@%s>" user domain) >>
  read_relay_rc r "^250" >>= fun res ->
  if res = `Ok then
    send_data r w context
  else
    return res

let send_from r w context =
  let (user,domain) = context.from in
  let from = match domain with
  | Some domain -> Printf.sprintf "MAIL FROM: <%s@%s>" user domain
  | None -> Printf.sprintf "MAIL FROM: <%s>" user 
  in
  write_relay w from >>
  read_relay_rc r "^250" >>= fun res ->
  if res = `Ok then
    send_rcptto r w context
  else
    return res

let rec read_ehlo r = function
  | `Ok capabilities ->
    begin
    read_relay r >>= function
    | None -> return `Failure
    | Some str ->
      if Regex.match_regex ~regx:"^250\\([ -]\\)\\(.*\\)$" str then (
        let capabilities = (Str.matched_group 2 str) :: capabilities in
        (* last response must be "250 " *)
        if (Str.matched_group 1 str) = "-" then 
          read_ehlo r (`Ok capabilities)
        else
          return (`Ok capabilities)
      ) else (
        return (`PermanentFailure str)
      )
    end
  | res -> return res

let send_ehlo r w (f:(string list -> [`Ok|`Failure|`PermanentFailure of string] Lwt.t)) =
  Lwt_unix.gethostname () >>= fun host ->
  write_relay w ("EHLO " ^ host) >>
  read_ehlo r (`Ok []) >>= function
  | `Ok capabilities ->
    f capabilities
  | `Failure -> return `Failure
  | `PermanentFailure err -> return (`PermanentFailure err)

let is_capability capabilities capability =
  List.exists (fun cap -> 
    Regex.match_regex ~case:false ~regx:capability cap
  ) capabilities 

let send_starttls sock r w context =
  write_relay w "STARTTLS" >>
  read_relay_rc r "^250\\|220" >>= fun res ->
  if res = `Ok then (
    let (_,domain,_) = List.hd context.rcpt in
    Log_.log `Info1 (Printf.sprintf "### starting tls to %s\n" domain);
    catch (fun () ->
    starttls_client domain sock () >>= fun (r,w) ->
    send_ehlo r w (fun _ -> send_from r w context)
    ) (fun ex -> 
        Log_.log `Error 
        (Printf.sprintf "### relay starttls exception: %s\n"
        (Printexc.to_string ex)); return (`PermanentFailure "starttls failed")
      )
  ) else
      return res

let greetings sock r w context =
  read_relay_rc r "^220" >>= function
  | `Ok -> send_ehlo r w (fun capabilities ->
    if is_capability capabilities "starttls" then
      send_starttls sock r w context
    else
      return (`PermanentFailure "starttls is required for relay"))
  | res -> return res

let send_relayed ip ports context =
  Log_.log `Info1 (Printf.sprintf "### relaying message to ip: %s\n" ip);
  let rec send = function
    | port :: tl ->
      begin
      Lwt.pick [
       Lwt_unix.sleep 30. >> return `Timeout;
       client_send (`Inet (ip,port)) (fun res sock ic oc ->
         greetings sock ic oc context
       ) `Ok >>= fun res -> return (`Ok res);
      ] >>= function
      | `Timeout -> send tl
      | `Ok res -> return res
      end
    | [] -> return (`PermanentFailure "failed establish connection to SMTP server default ports")
  in
  send ports

let failed on_failure err context =
  Lwt_unix.gethostname () >>= fun host ->
  let (user,domain) = context.from in
  let (rcpt_user,rcpt_domain,_) = List.hd context.rcpt in
  let domain = match domain with
  | None -> host
  | Some domain -> domain
  in
  let from = ("mailer-daemon", Some domain) in
  let rcpt = [(user,domain,`None)] in
  let len = Buffer.length context.buff in
  let msg = 
"From: Mail Delivery Subsystem <mailer-daemon@" ^ domain ^ ">
To: " ^ user ^ "@" ^ domain ^ "
Subject: Delivery Status Notification (Failure)
Message-ID: <" ^ (string_of_float (Unix.gettimeofday())) ^ "@" ^ domain ^ ">
Date: " ^ (Dates.date_time_to_email (Dates.ImapTime.now())) ^ "
Content-Type: text/plain;charset=UTF-8

Delivery to the following recipient failed permanently:

  " ^ rcpt_user ^ "@" ^ rcpt_domain ^ "\n\n" ^ err ^ "\n

----- Original message (truncated) -----\n\n" ^ 
  Buffer.sub context.buff 0 (if len > 1000 then 1000 else len) in
  Buffer.clear context.buff;
  Buffer.add_string context.buff msg;
  on_failure {context with from;rcpt}

(* relay the message 
 * assume only one receipient for now TBD *)
let rec relay context on_failure =
  Log_.log `Info2 "### relaying message\n";
  let (user, domain, relay_rec) = List.hd context.rcpt in
  let send relay_rec ip ports =
    send_relayed ip ports context >>= function 
    | `Ok -> 
      Log_.log `Info2 "### relay succeeded\n";
      return ()
    | `Failure ->
      Log_.log `Info2 "### relay failure, retrying\n";
      Lwt_unix.sleep 60. >>
      relay {context with rcpt = [(user, domain, relay_rec)]} on_failure
    | `PermanentFailure err -> 
      Log_.log `Info2 "### permanent relay failure\n";
      failed on_failure err context
  in
  match relay_rec with
  | `MXRelay relay_rec ->
    begin
      match relay_rec with 
        (* is trying on one interface ok? TBD *)
      | (pri,ips) :: tl -> send (`MXRelay tl) (List.hd ips) [25;587;2587]
      | [] ->
        Log_.log `Info2 "### mx rr records are empty, can't relay\n"; 
        failed on_failure "failed establish connection to SMTP server interfaces" context
    end
  | `DirectRelay (ip,port) -> send `None ip [port]
  | `None ->  
    Log_.log `Info2 "### no mx rr/direct records, can't relay\n"; return ()
