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
open Server_config

let timeout = 120.

(*
 * write to the server
 *)
let write_relay w msg =
  Log_.log `Info3 (Printf.sprintf "<-- relayed: %s\n" msg);
  Lwt_io.write w (msg ^ "\r\n") 

(*
 * read response from the server
 *)
let read_relay r =
  Lwt.pick [
    (Lwt_unix.sleep timeout >> return None);
    Lwt_io.read_line_opt r;
  ] >>= function
  | Some str -> Log_.log `Info3 (Printf.sprintf "--> relayed: %s\n" str); return (Some str)
  | None -> return None

(*
 * read response from the server, 
 * match the response with the regex for Ok
 *)
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

(*
 * add 'stun' header
 *)
let add_stun_header content context =
  (* if not master then add 'Received:' trace header *)
  match context.config.master with
  | Some master when master <> "localhost" && master <> "127.0.0.1" ->
    Stun_maint.add_header ~content ~master
  | None ->
    Stun_maint.add_header ~content ~master:""
  | _ -> content

(*
 * send data to the server
 *)
let send_data r w context =
  write_relay w "DATA" >>
  read_relay_rc r "^250\\|354" >>= fun res ->
  if res = `Ok then (
    let content =  add_stun_header (Buffer.contents context.buff) context in
    let lwt_buff = Lwt_bytes.of_bytes content in
    let dc = Lwt_io.of_bytes ~mode:Lwt_io.input lwt_buff in
    (* send one line of data at a time *)
    let rec send () =
      catch (fun () ->
        Lwt_io.read_line_opt dc >>= function
        | Some str -> write_relay w str >> send ()
        | None -> write_relay w "." >>
          read_relay_rc r "^250" >>= fun res ->
          write_relay w "QUIT" >>
          return res
      )
      ( fun ex ->
        let msg = Printexc.to_string ex in
        Log_.log `Error (Printf.sprintf "### failed to send data %s\n" msg); 
        return (`PermanentFailure msg)
      )
    in
    send ()
  ) else (
    return res
  )

(* send rcptto *)
let send_rcptto r w context =
  let (user,domain,_) = List.hd context.rcpt in
  write_relay w (Printf.sprintf "RCPT TO: <%s@%s>" user domain) >>
  read_relay_rc r "^250" >>= fun res ->
  if res = `Ok then
    send_data r w context
  else
    return res

(* if there is a master then replace the domain with the master domain *)
let get_from context =
  let (user,domain) = context.from in
  let domain_part =
    match context.config.master with
    | Some m when m <> "localhost" && m <> "127.0.0.1" -> "@" ^ m
    | _ ->
      begin
      match domain with
      | Some domain -> "@" ^ domain
      | None -> ""
      end
  in
  Printf.sprintf "MAIL FROM: <%s%s>" user domain_part

(* send from *)
let send_from r w context =
  let from = get_from context in
  write_relay w from >>
  read_relay_rc r "^250" >>= fun res ->
  if res = `Ok then
    send_rcptto r w context
  else
    return res

(* read ehlo response *)
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

(* send ehlo *)
let send_ehlo r w (f:(string list -> [`Ok|`Failure|`PermanentFailure of string] Lwt.t)) =
  Lwt_unix.gethostname () >>= fun host ->
  write_relay w ("EHLO " ^ host) >>
  read_ehlo r (`Ok []) >>= function
  | `Ok capabilities ->
    f capabilities
  | `Failure -> return `Failure
  | `PermanentFailure err -> return (`PermanentFailure err)

(* check for capability property *)
let is_capability capabilities capability =
  List.exists (fun cap -> 
    Regex.match_regex ~case:false ~regx:capability cap
  ) capabilities 

(* starttls with the server *)
let send_starttls ip sock r w context =
  write_relay w "STARTTLS" >>
  read_relay_rc r "^250\\|220" >>= fun res ->
  if res = `Ok then (
    Log_.log `Info1 (Printf.sprintf "### starting tls to %s\n" ip);
    catch (fun () ->
    starttls_client ip sock () >>= fun (r,w) ->
    send_ehlo r w (fun _ -> send_from r w context)
    ) (fun ex -> 
        Log_.log `Error 
        (Printf.sprintf "### relay starttls exception: %s\n"
        (Printexc.to_string ex)); return (`PermanentFailure "starttls failed")
      )
  ) else
      return res

(* start state machine *)
let greetings ip sock r w context =
  read_relay_rc r "^220" >>= function
  | `Ok -> send_ehlo r w (fun capabilities ->
    if is_capability capabilities "starttls" then
      send_starttls ip sock r w context
    else
      return (`PermanentFailure "starttls is required for relay"))
  | res -> return res

(* try to send to available ports until success *)
let send_relayed ip ports context =
  let rec send = function
    | port :: tl ->
      Log_.log `Info1 (Printf.sprintf "### relaying message to ip: %s, port: %d\n" ip port);
      begin
      Lwt.pick [
       Lwt_unix.sleep timeout >> return `Timeout;
       (catch (fun () -> client_send (`Inet (ip,port)) (fun res sock ic oc ->
         greetings ip sock ic oc context) `Ok >>= fun res -> return (`Ok res)) 
       (fun ex -> Log_.log `Error (Printf.sprintf "### failed to send %s\n"
          (Printexc.to_string ex)); return `Timeout (* try another port *))
       )
      ] >>= function
      | `Timeout -> 
        Log_.log `Info1 "### relay timeout\n";
        send tl
      | `Ok res -> 
        Log_.log `Info1 "### relay succeeded\n";
        return res
      end
    | [] -> return (`PermanentFailure "failed establish connection to SMTP server default ports")
  in
  send ports

(* send message on failure back to the client *)
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

(* relay the message based on the priority list of available addresses until
 * success assume only one receipient for now TBD *)
let rec relay try_stun context on_failure =
  let open Stun_maint in
  Log_.log `Info3 "### relaying message\n";
  let (user, domain, relay_rec) = List.hd context.rcpt in
  let send relay_rec ip ports =
    send_relayed ip ports context >>= function 
    | `Ok -> 
      Log_.log `Info3 "### relay succeeded\n";
      return ()
    | `Failure ->
      (* retry *)
      Log_.log `Info3 "### relay failure, retrying\n";
      Lwt_unix.sleep timeout >>
      relay false {context with rcpt = [(user, domain, relay_rec)]} on_failure
    | `PermanentFailure err -> 
      Log_.log `Info3 "### permanent relay failure\n";
      failed on_failure err context
  in
  (* try direct send to the receiver on the same net first *)
  match Stun_maint.match_stun_records (Buffer.contents context.buff) domain with
  | Some record when try_stun -> 
    Log_.log `Info3 (Printf.sprintf 
      "### relaying based on stun: private %s, public %s, ports %s\n" 
      record.privateaddr record.pubaddr (Stun_maint.ports_to_string record.ports));
    send relay_rec record.privateaddr record.ports
  | _ ->
    (* try based on priority list, firs MX record, then direct (if specified by IP)
     *)
    match relay_rec with
    | `MXRelay relay_rec ->
      begin
        match relay_rec with 
          (* is trying on one interface ok? TBD *)
        | (pri,ips) :: tl -> 
          Log_.log `Info3 
            (Printf.sprintf "### MXRelay to %s:25,587,2587\n" (List.hd ips)); 
          send (`MXRelay tl) (List.hd ips) [25;587;2587]
        | [] ->
          Log_.log `Info3 "### mx rr records are empty, can't relay\n"; 
          failed on_failure "failed establish connection to SMTP server interfaces" context
      end
    | `DirectRelay (ip,ports) -> 
      Log_.log `Info3 
        (Printf.sprintf "### DirectRelay to %s:%s\n" ip (Stun_maint.ports_to_string ports)); 
      send `None ip ports
    | `None ->  
      Log_.log `Info3 "### no mx rr/direct records, can't relay\n"; return ()
