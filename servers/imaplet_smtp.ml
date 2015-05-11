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
open Server_config
open Account
open Dates

exception InvalidCmd

type netio = Lwt_io.input_channel * Lwt_io.output_channel*
  [`Reg of Lwt_unix.file_descr|`Ssl|`Starttls of Lwt_unix.file_descr]

type cmd_context = {config:imapConfig;auth:bool;grttype:[`Helo|`Ehlo|`Rset]; io:netio;
  from:string;rcpt:string list;buff:Buffer.t}

let _ = Log_.set_log "smtplet.log"

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
let send_to_imap addr context =
  Log_.log `Info1 "smtp: sending to imap\n";
  let _from = context.from in
  let msg = Buffer.contents context.buff in
  let _to = List.nth context.rcpt 0 in
  let msg = add_postmark _from msg in
  client_send addr (fun inchan outchan ->
    let write buff = Lwt_io.write outchan buff >>= fun () -> Lwt_io.flush outchan in
    let read () = Lwt_io.read_line inchan in
    write ("a lappend " ^ _to ^ " INBOX {" ^ (string_of_int (String.length msg)) ^ "+}\r\n") >>= fun () ->
    write msg >>= fun () ->
    read () >>= fun resp -> (* a OK APPEND completed *)
    write "a logout\r\n" >>= fun () ->
    read () >>= fun resp -> (* * BYE *)
    return ()
  )

let write context msg = 
  Log_.log `Info1 (Printf.sprintf "<-- %s\n" msg);
  let (_,w,_) = context.io in
  Lwt_io.write w (msg ^ "\r\n") 

let read context =
  let (r,_,_) = context.io in
  catch (fun () ->
    Lwt.pick [
      (Lwt_unix.sleep  context.config.smtp_idle_max >> 
      Lwt_unix.gethostname () >>= fun host ->
      write context ("421 4.4.2 " ^ host ^ " Error: timeout exceeded") >>
      return None);
      Lwt_io.read_line_opt r;
    ]
  ) (fun ex -> Log_.log `Error (Printf.sprintf "smtp:read exception %s\n" (Printexc.to_string ex));
  return None)

let buffer_ends buffer str =
  let str_len = Bytes.length str in
  let buf_len = Buffer.length buffer in
  if str_len > buf_len then
    false
  else
    (Buffer.sub buffer (buf_len-str_len) str_len) = str 

let syntx_helo next_state ~msg str context =
  if List.exists (fun s -> s = `Helo) next_state = false then (
    write context msg >>
    return `Next
  ) else if Regex.match_regex ~regx:"^[ \t]+\\([^ \t]\\)+$" str then (
    write context "250 OK" >>
    return `Helo
  ) else (
    write context "501 5.5.2 Syntax: HELO hostname" >>
    return `Next
  )

let starttls_required context =
  let (_,_,s) = context.io in
  match s with
  | `Reg _ when context.config.starttls -> true
  | _ -> false

let auth_required context =
  if starttls_required context then
    false
  else if context.grttype = `Ehlo then
    context.auth = false
  else
    false

let syntx_ehlo next_state ~msg str context =
  if List.exists (fun s -> s = `Ehlo) next_state = false then (
    write context msg >>
    return `Next
  ) else if Regex.match_regex ~regx:"^[ \t]+\\([^ \t]+\\)$" str then (
    Lwt_unix.gethostname () >>= fun host ->
    let host = "250-" ^ host in
    let cap = ["250-ENHANCEDSTATUSCODES";"250 VRFY"] in
    let tls = starttls_required context in
    let auth = auth_required context in
    let cap =
      if tls then
        host :: ("250-STARTTLS" :: ("250-AUTH PLAIN LOGIN" :: cap))
      else if auth then
        host :: ("250-AUTH PLAIN LOGIN" :: cap)
      else
        host :: cap
    in
    Lwt_list.iter_s (fun c -> write context c) cap >>
    return `Ehlo
  ) else (
    write context "501 5.5.2 Syntax: EHLO hostname" >>
    return `Next
  )

let syntx_rset next_state ~msg context =
  if List.exists (fun s -> s = `Rset) next_state = false then (
    write context msg >>
    return `Next
  ) else (
    write context "250 OK" >>
    return `Rset
  )

let syntx_noop next_state ~msg context =
  if List.exists (fun s -> s = `Noop) next_state = false then (
    write context msg >>
    return `Next
  ) else (
    write context "250 OK" >> 
    return `Next
  )

let syntx_vrfy next_state ~msg str context =
  if List.exists (fun s -> s = `Vrfy) next_state = false then (
    write context msg >>
    return `Next
  ) else if Regex.match_regex ~regx:"^[ \t]+<?\\([^ @<>\t]+\\)$" str then (
    let user = Str.matched_group 1 str in
    authenticate_user user () >>= fun auth ->
    if auth then (
      write context ("252 " ^ user) >>
      return `Authenticated
    ) else (
      write context ("550 5.7.8 " ^ user ^ "Recipient address rejected: User unknown in local recipient table") >>
      return `Next
    )
  ) else (
    write context "501 5.5.2 Bad recipient address syntax" >>
    return `Next
  ) 

let syntx_quit context =
  write context "250 OK" >>
  return `Quit

let syntx_from next_state ~msg cmd context =
  if List.exists (fun s -> s = `MailFrom) next_state = false then (
    write context msg >>
    return `Next
  ) else if Regex.match_regex ~case:false ~regx:"^[ \t]+FROM:[ \t]*<?\\([^ <>@\t]+\\)" cmd then (
    write context "250 OK" >>
    return (`MailFrom (Str.matched_group 1 cmd))
  ) else (
    write context "501 5.5.2 Syntax: MAIL FROM:<address>" >>
    return `Next
  )

let syntx_rcpt next_state ~msg cmd context =
  if List.exists (fun s -> s = `RcptTo) next_state = false then (
    write context msg >>
    return `Next
  ) else if Regex.match_regex ~case:false ~regx:"^[ \t]+TO:[ \t]*<?\\([^ <>@\t]+\\)" cmd then (
    let user = Str.matched_group 1 cmd in
    authenticate_user user () >>= fun auth ->
    if auth then (
      write context "250 OK" >>
      return (`RcptTo user)
    ) else (
      write context "550 5.7.8 : Recipient address rejected: User unknown in local recipient table" >>
      return `Next
    )
  ) else (
    write context "501 5.5.2 Syntax: RCPT TO:<address>" >>
    return `Next
  )

let syntx_data next_state ~msg cmd context =
  if List.exists (fun s -> s = `Data) next_state = false then (
    write context msg >>
    return `Next
  ) else if Regex.match_regex ~regx:"^[ \t]*$" cmd then (
    write context "354 End data with <CR><LF>.<CR><LF>" >>
    return `Data
  ) else (
    write context "501 5.5.2 Syntax: DATA" >>
    return `Next
  )

let syntx_starttls next_state ~msg cmd context =
  if List.exists (fun s -> s = `Starttls) next_state = false then (
    write context msg >>
    return `Next
  ) else if Regex.match_regex ~regx:"^[ \t]*$" cmd = false then (
    write context "501 5.5.2 Syntax error" >>
    return `Next
  ) else (
    let (_,_,s) = context.io in
    match s with
    | `Reg sock -> 
      write context "220 ready to start TLS" >>
      return (`Starttls sock)
    | _ ->
      write context "454 4.5.0 TLS not available due to local problem" >>
      return `Next
  )

let syntx_auth next_state ~msg cmd context =
  if List.exists (fun s -> s = `AuthPlain || s = `AuthLogin) next_state = false then (
    write context msg >>
    return `Next
  ) else if Regex.match_regex ~case:false 
    ~regx:"^[ \t]+PLAIN[ \t]+\\([^ \t]+\\)$" cmd then (
    return (`AuthPlain (Str.matched_group 1 cmd))
  ) else if Regex.match_regex ~case:false ~regx:"^[ \t]+PLAIN[ \t]*$" cmd then (
    write context "334 " >> (* request the text *)
    return (`AuthPlain "")
  ) else if Regex.match_regex ~case:false ~regx:"^[ \t]+LOGIN[ \t]*$" cmd then (
    write context "334 VXNlcm5hbWU6" >>  (* 334 Username *)
    return `AuthLogin
  ) else (
    write context "501 5.5.2 Syntax error" >>
    return `Next
  )

let next ?(isdata=false) ?(msg="503 5.5.1 Command out of sequence") ~next_state context =
  read context >>= function
  | None -> Log_.log `Debug "smtp: client terminated\n";return `Quit
  | Some str -> Log_.log `Info3 (Printf.sprintf "--> %s\n" str);
  let domatch ?(tmpl="\\(.*\\)$") rx =
    Regex.match_regex ~case:false ~regx:("^[ \t]*" ^ rx ^ tmpl) str in
  let get () = try Str.matched_group 1 str with _ -> "" in
  if isdata then (
    return (`DataStream str)
  ) else if domatch ~tmpl:"\\([ \t]+[^ \t]+?\\)?$" "HELO" then (
    syntx_helo next_state ~msg (get ()) context
  ) else if domatch ~tmpl:"\\([ \t]+[^ \t]+?\\)?$" "EHLO" then (
    syntx_ehlo next_state ~msg (get ()) context
  ) else if domatch ~tmpl:"\\([ \t]+[^ \t]+?.*\\)?$" "MAIL" then (
    syntx_from next_state ~msg (get ()) context
  ) else if domatch ~tmpl:"\\([ \t]+[^ \t]+?.*\\)?$" "RCPT" then (
    syntx_rcpt next_state ~msg (get ()) context
  ) else if domatch ~tmpl:"\\([ \t]*.*\\)$" "DATA" then (
    syntx_data next_state ~msg (get()) context
  ) else if domatch ~tmpl:"\\([ \t]+[^ \t]+?\\)?$" "NOOP" then (
    syntx_noop next_state ~msg context
  ) else if domatch ~tmpl:"\\([ \t]+[^ \t]+?\\)?$" "RSET" then (
    syntx_rset next_state ~msg context
  ) else if domatch ~tmpl:"\\([ \t]+[^ \t]+?\\)?$" "STARTTLS" then (
    syntx_starttls next_state ~msg (get ()) context
  ) else if domatch ~tmpl:"\\([ \t]+[^ \t]+?\\)?$" "QUIT" then (
    syntx_quit context
  ) else if domatch ~tmpl:"\\([ \t]+[^ \t]+?\\)?$" "VRFY" then (
    syntx_vrfy next_state ~msg (get ()) context
  ) else if domatch ~tmpl:"\\([ \t]+.*]+?\\)?$" "AUTH" then (
    syntx_auth next_state ~msg (get ()) context
  ) else (
    write context "502 5.5.1 Error: command not recognized" >>
    return `Next
  )

let all state context = function
  | `Quit -> return `Quit
  | `Rset -> return `Rset
  | `Helo -> return `Helo
  | `Ehlo -> return `Ehlo
  | `Next -> return (`State (state,context))
  | _ -> return (`State (state,context))

let rec datastream context =
  Log_.log `Debug "smtp: starting datastream\n";
  next ~isdata:true ~next_state:[`DataStream] context >>= function
  | `DataStream str ->
    if buffer_ends context.buff "\r\n" && str = "." then (
      catch (fun () ->
        send_to_imap (`Unix (Filename.concat Install.data_path "sock/smtp")) context >>
        write context "250 OK"
      ) 
      (fun ex -> write context "554 5.5.0 Transaction failed") >> 
      return `Rset
    ) else (
      Buffer.add_string context.buff (str ^ "\r\n");
      return (`State (datastream, context))
    )
  (* anything is data until done, would not get to this state *)
  | _ -> return `Rset

let rec rcptto context =
  Log_.log `Debug "smtp: starting rcptto\n";
  next ~next_state:[`RcpTo;`Data;`Vrfy;`Noop;`Helo;`Ehlo;`Rset] context >>= function
  | `RcptTo r -> return (`State (rcptto, {context with rcpt = r :: context.rcpt}))
  | `Data -> return (`State (datastream,context))
  | cmd -> all (rcptto) context cmd

let rec mailfrom context =
  Log_.log `Debug "smtp: starting mailfrom\n";
  next ~next_state:[`RcptTo;`Vrfy;`Noop;`Helo;`Ehlo;`Rset] context >>= function 
  | `RcptTo r -> return (`State (rcptto, {context with rcpt = r :: context.rcpt}))
  | cmd -> all (mailfrom) context cmd

let authenticate text ?password context =
  begin
  match password with 
  | None -> plain_auth text
  | Some password ->
    authenticate_user ~b64:true text ~password ()
  end >>= fun auth ->
  if auth then (
    write context "235 2.7.0 Authentication successful" >>
    return `Authenticated
  ) else (
    write context "535 5.7.8 Authentication failed" >>
    return `Rset
  )

(* auth is done right after starttls *)
let rec authplain context =
  Log_.log `Debug "smtp: starting authplain\n";
  next ~isdata:true ~next_state:[`DataStream] context >>= function
  | `DataStream text ->
    authenticate text context
  | _ -> return `Rset (* will not get here, cause of DataStream *)

let rec authlogin user context =
  Log_.log `Debug "smtp starting authlogin\n";
  next ~isdata:true ~next_state:[`DataStream] context >>= function
  | `DataStream text -> 
    begin
    match user with
    | None ->
      write context "334 UGFzc3dvcmQ6" >> (* 334 Password *)
      return (`State (authlogin (Some text), context))
    | Some user -> 
      authenticate user ~password:text context
    end
  | _ -> return `Rset (* will not get here, cause of DataStream *)

let rec helo context =
  Log_.log `Debug "smtp: starting helo\n";
  next ~next_state:[`MailFrom;`Ehlo;`Helo;`Noop;`Vrfy;] context >>= function
  | `MailFrom from -> return (`State (mailfrom, {context with from}))
  | cmd -> all (helo) context cmd

let doauth context = function
  | `AuthPlain text -> 
    if text <> "" then (
      authenticate text context
    ) else (
      return (`State (authplain,context))
    )
  | `AuthLogin -> return (`State (authlogin None ,context))

let rec auth context =
  Log_.log `Debug "smtp starting auth\n";
  next ~next_state:[`Ehlo;`Helo;`AuthPlain;`AuthLogin]
      ~msg:"530 5.7.0 Must issue a AUTH command first" context >>= function
  | `AuthPlain text as cmd -> doauth context cmd
  | `AuthLogin as cmd -> doauth context cmd
  | cmd -> all (auth) context cmd

let rec ehlo context =
  Log_.log `Debug "smtp: starting ehlo\n";
  let (next_state,msg) = 
    if starttls_required context then
      ([`Ehlo;`Helo;`Starttls],"530 5.7.0 Must issue a STARTTLS command first")
    else if auth_required context then
      ([`Ehlo;`Helo;`AuthPlain;`AuthLogin],"530 5.7.0 Must issue a AUTH command first")
    else
      ([`MailFrom;`Ehlo;`Helo;`Noop;`Rset;`Vrfy],"503")
  in
  next ~next_state ~msg context >>= function
  | `MailFrom from -> return (`State (mailfrom, {context with from}))
  | `Starttls sock ->
    starttls context.config sock () >>= fun (r,w) ->
    return (`State (auth, {context with io = (r,w,`Starttls sock)}))
  | `AuthPlain text as cmd -> doauth context cmd
  | `AuthLogin as cmd -> doauth context cmd
  | cmd -> all (ehlo) context cmd

let rec start context =
  Log_.log `Debug "smtp: starting state\n";
  let (next_state,msg) =
    if starttls_required context then
      ([`Ehlo;`Helo;`Starttls],"530 5.7.0 Must issue a STARTTLS command first")
    else 
      ([`Ehlo;`Helo;`Rset;`Noop;`Vrfy],"503")
  in
  next ~next_state ~msg context >>= function
  | `Starttls sock ->
    starttls context.config sock () >>= fun (r,w) ->
    return (`State (auth, {context with io = (r,w,`Starttls sock)}))
  | cmd -> all (start) context cmd

let greeting context =
  write context "220 server ESMTP smtplet" >>
  let rec run state context =
    state context >>= function
    | `Quit -> return ()
    | `Ehlo -> run (ehlo) {context with grttype=`Ehlo;buff=Buffer.create 100}
    | `Authenticated -> run (ehlo) {context with auth=true;grttype=`Ehlo;buff=Buffer.create 100}
    | `Helo -> run (helo) {context with grttype=`Helo;buff=Buffer.create 100}
    | `Rset -> 
      if context.grttype = `Ehlo then
        run (ehlo) {context with buff=Buffer.create 100}
      else
        run (helo) {context with buff=Buffer.create 100}
    | `State (state,context) -> run (state) context
    | _ -> write context "503 5.5.1 Command out of sequence" >> run (state) context
  in
  run (start) context

let mkcontext config sock inchan outchan =
  let io =
    match sock with
    | None -> (inchan,outchan,`Ssl)
    | Some sock -> (inchan,outchan,`Reg sock)
  in
  {config;auth=false;grttype=`Rset;io;from="";rcpt=[];buff=Buffer.create 100}

let _ =
  Lwt_main.run (
    (* temp, overwrite ssl/starttls with smtp_ssl/smtp_starttls *)
    let config = {srv_config with
      ssl=srv_config.smtp_ssl;starttls=srv_config.smtp_starttls} in
    Log_.log `Info1 (Printf.sprintf "imaplet_smtp: started %s %s:%d:%b:%b\n" 
      (ImapTime.to_string (ImapTime.now()))
      config.smtp_addr config.smtp_port config.ssl config.starttls);
    server (`Inet (config.smtp_addr,config.smtp_port)) config 
    (fun sock inchan outchan ->
      greeting (mkcontext config sock inchan outchan)
    ) (fun ex -> Log_.log `Error (Printf.sprintf "imaplet_smtp: exception %s\n"
    (Printexc.to_string ex)); return ())
  )
