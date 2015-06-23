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
open Smtplet_context

exception InvalidCmd

let _ = Log_.set_log "smtplet.log"

let imap_addr = `Unix (Filename.concat Install.data_path "sock/smtp")

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
 * should it be SSL? TBD 
 *)
let send_to_imap addr context =
  Log_.log `Info1 "smtp: sending to imap\n";
  let (_from,domain) = context.from in
  let msg = Stun_maint.cache_stun_records domain (Buffer.contents context.buff) in
  Log_.log `Info3 (Printf.sprintf "%s" (if String.length msg > 1000 then
    (String.sub msg 0 1000) else (msg)));
  let (_to,_,_) = List.hd context.rcpt in
  let up = context.auth in (* maybe don't need this TBD *)
  let pswd = 
    match up with
    | Some (_,pswd) -> (match pswd with |Some pswd -> " " ^ pswd|None->"")
    | None -> ""
  in
  let msg = add_postmark _from msg in
  client_send addr (fun _ _ inchan outchan ->
    let write buff = Lwt_io.write outchan buff >>= fun () -> Lwt_io.flush outchan in
    let read () = Lwt_io.read_line inchan in
    write ("a lappend " ^ _to ^ pswd ^ " INBOX {" ^ (string_of_int (String.length msg)) ^ "+}\r\n") >>= fun () ->
    write msg >>= fun () ->
    read () >>= fun resp -> (* a OK APPEND completed *)
    write "a logout\r\n" >>= fun () ->
    read () >>= fun resp -> (* * BYE *)
    return ()
  ) ()

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
    return `NextOutOfSeq
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

(* if starttls required then have to do starttls first
 *)
let auth_required context =
  if starttls_required context then
    false
  else if context.grttype = `Ehlo then
    (* need to figure out when to make auth
     * required if at all, since the relay would not have
     * the password *)
    (*context.auth = None *) false
  else
    false

let syntx_ehlo next_state ~msg str context =
  if List.exists (fun s -> s = `Ehlo) next_state = false then (
    write context msg >>
    return `NextOutOfSeq
  ) else if Regex.match_regex ~regx:"^[ \t]+\\([^ \t]+\\)$" str then (
    Lwt_unix.gethostname () >>= fun host ->
    let host = "250-" ^ host in
    let cap = ["250-ENHANCEDSTATUSCODES";"250 VRFY"] in
    let tls = starttls_required context in
    let auth = auth_required context in
    let cap =
      if tls then
        host :: ("250-STARTTLS" :: ("250-AUTH PLAIN LOGIN" :: cap))
      else (*if auth then tmp *)
        host :: ("250-AUTH PLAIN LOGIN" :: cap)
      (*else
        host :: cap*)
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
    return `NextOutOfSeq
  ) else (
    write context "250 OK" >>
    return `Rset
  )

let syntx_noop next_state ~msg context =
  if List.exists (fun s -> s = `Noop) next_state = false then (
    write context msg >>
    return `NextOutOfSeq
  ) else (
    write context "250 OK" >> 
    return `Next
  )

let syntx_vrfy next_state ~msg str context =
  if List.exists (fun s -> s = `Vrfy) next_state = false then (
    write context msg >>
    return `NextOutOfSeq
  ) else if Regex.match_regex ~regx:"^[ \t]+<?\\([^ <>\t]+\\)>?$" str then (
    let user = Str.matched_group 1 str in
    authenticate_user user () >>= fun (_,_,auth) ->
    if auth then (
      write context ("252 " ^ user) >>
      return `Next
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
    return `NextOutOfSeq
  ) else if Regex.match_regex ~case:false ~regx:"^[ \t]+FROM:[ \t]*<?\\([^ <>@\t]+\\)\\(@\\([^>]*\\)\\)?" cmd then (
    let user = Str.matched_group 1 cmd in
    let domain = try Some (Str.matched_group 3 cmd) with Not_found -> None in
    write context "250 OK" >>
    return (`MailFrom (user,domain))
  ) else (
    write context "501 5.5.2 Syntax: MAIL FROM:<address>" >>
    return `Next
  )

let in_my_domain context domain =
  let interface = context.config.smtp_addr in
  let mydomain = context.config.domain in
  Log_.log `Info2 
    (Printf.sprintf "### detecting send to domain %s on interface %s, my domain %s\n" 
    domain interface mydomain) ;
  Utils.get_interfaces () >>= fun my_ips ->
  begin
  let dexists domains domain = 
    List.exists (fun d -> d = domain) (Str.split (Str.regexp ";") domains) in
  if dexists context.config.domain domain then (
    return my_ips (* it's my domain *)
  ) else if (try let _ = Unix.inet_addr_of_string domain in true with _ -> false) then (
    return [domain] (* already ip *)
  ) else (
    Imaplet_dns.gethostbyname domain
  )
  end >>= fun domain_ips ->
  let ip = (List.fold_left (fun acc ip -> 
     if List.exists (fun i -> i = ip) my_ips then (
       (Some ip)
     ) else
       acc
    ) None domain_ips)
  in
  match ip with
  | None -> 
    Log_.log `Info2 (Printf.sprintf "### didn't find overlaping ip\n");
    return `No
  | Some ip ->
    Log_.log `Info2 (Printf.sprintf "### found overlaping ip %s\n" ip);
    if interface = "0.0.0.0" || interface = ip then
      return `Yes
    else
      return `NoInterface

let syntx_rcpt next_state ~msg cmd context =
  if List.exists (fun s -> s = `RcptTo) next_state = false then (
    write context msg >>
    return `NextOutOfSeq
  ) else if Regex.match_regex ~case:false ~regx:"^[ \t]+TO:[ \t]*<?\\([^ <>@\t]+\\)@\\([^>]*\\)" cmd then (
    let user = Str.matched_group 1 cmd in
    let domain =  Str.matched_group 2 cmd in
    in_my_domain context domain >>= fun res ->
    if res = `NoInterface then ( (* sent to wrong interface *)
      write context "550 5.1.2 : Not accepting on this network interface" >>
      return `Next
    ) else if res = `Yes then (
      authenticate_user user () >>= fun (_,_,auth) ->
      if auth then (
        write context "250 OK" >>
        return (`RcptTo (user,domain,`None))
      ) else (
        write context "550 5.7.8 : Recipient address rejected: User unknown in local recipient table" >>
        return `Next
      )
    (* have to relay, only allow authenticated users to relay *)
    (* ) else if context.auth = None then (
      write context "550 5.7.1 : Authentication required" >>
      return `Next *)
    ) else (
      (* from user must have the account *)
      let valid_from context =
        let (user,domain) = context.from in
        authenticate_user user () >>= fun (_,_,auth) ->
        if auth then (
          if domain = None then
            return true
          else (
            in_my_domain context (Utils.option_value_exn domain) >>= function
            | `Yes -> return true
            | _ -> return false
          )
        ) else (
          return false
        )
      in
      valid_from context >>= fun valid ->
      if valid then (
        Log_.log `Info1 (Printf.sprintf "### scheduling to relay to domain %s\n" domain);
        Imaplet_dns.resolve domain >>= fun mx_rr ->
        (* might be relaying directly to another device in the same network,
         * need a way to figure it out?
         *)
        if List.length mx_rr > 0 then (
          write context "250 OK" >>
          return (`RcptTo (user,domain,`MXRelay mx_rr))
        ) else if (try let _ = Unix.inet_addr_of_string domain in true with _ -> false) then (
          (* temp work around for direct send - if the address is ip then use
           * it, need to make a more general case, look at the headers, check if
           * STUN mapped address in the header (additional header X-?) matches
           * this STUN mapped address *)
          write context "250 OK" >>
          return (`RcptTo (user,domain, `DirectRelay (domain,2587)))
        ) else (
          write context "550 5.1.2 : Invalid domain" >>
          return `Next
        )
      ) else (
        write context "550 5.7.8 : From address rejected: User unknown in local
 recipient table or invalid domain" >>
        return `Next
      )
    )
  ) else (
    write context "501 5.5.2 Syntax: RCPT TO:<address>" >>
    return `Next
  )

let syntx_data next_state ~msg cmd context =
  if List.exists (fun s -> s = `Data) next_state = false then (
    write context msg >>
    return `NextOutOfSeq
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
    return `NextOutOfSeq
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
    return `NextOutOfSeq
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
    return `NextAuthError
  )

let next ?(isdata=false) ?(msg="503 5.5.1 Command out of sequence") ~cur_state ~next_state context =
  read context >>= function
  | None -> Log_.log `Debug "smtp: client terminated\n";return `Quit
  | Some str -> 
  begin
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
  end >>= fun res ->
  let log_msg =
  match res with
  | `AuthPlain _ -> "AUTH PLAIN ..."
  | `AuthLogin -> "AUTH LOGIN ..."
  | `NextAuthError -> "AUTH error ..."
  | `NextOutOfSeq when (String.lowercase (String.sub str 0 4)) = "auth" -> "AUTH out of seq ..."
  | `DataStream _ when cur_state = `AuthPlain || cur_state = `AuthLogin -> "AUTH data ..."
  | _ -> str
  in
  Log_.log `Info3 (Printf.sprintf "--> %s\n" log_msg);
  return res

let all state context = function
  | `Quit -> return `Quit
  | `Rset -> return `Rset
  | `Helo -> return `Helo
  | `Ehlo -> return `Ehlo
  | `Next -> return (`State (state,context))
  | _ -> return (`State (state,context))

let rec datastream context =
  Log_.log `Debug "smtp: starting datastream\n";
  next ~isdata:true ~cur_state:`DataStream ~next_state:[`DataStream] context >>= function
  | `DataStream str ->
    if buffer_ends context.buff "\r\n" && str = "." then (
      let (_,_,relay_rec) = List.hd context.rcpt in
      if relay_rec = `None then (
        catch (fun () ->
         send_to_imap imap_addr context >>
         write context "250 OK"
        )  
        (fun ex -> write context "554 5.5.0 Transaction failed") >> 
        return `Rset
      ) else (
        async (fun () -> Smtplet_relay.relay context.config.stun_header context (send_to_imap imap_addr));
        write context "250 OK" >>
        return `Rset
      )
    ) else (
      Buffer.add_string context.buff (str ^ "\r\n");
      return (`State (datastream, context))
    )
  (* anything is data until done, would not get to this state *)
  | _ -> return `Rset

let rec rcptto context =
  Log_.log `Debug "smtp: starting rcptto\n";
  next ~cur_state:`RcptTo ~next_state:[`RcpTo;`Data;`Vrfy;`Noop;`Helo;`Ehlo;`Rset] context >>= function
  | `RcptTo r -> return (`State (rcptto, {context with rcpt = r :: context.rcpt}))
  | `Data -> return (`State (datastream,context))
  | cmd -> all (rcptto) context cmd

let rec mailfrom context =
  Log_.log `Debug "smtp: starting mailfrom\n";
  next ~cur_state:`MailFrom ~next_state:[`RcptTo;`Vrfy;`Noop;`Helo;`Ehlo;`Rset] context >>= function 
  | `RcptTo r -> return (`State (rcptto, {context with rcpt = r :: context.rcpt}))
  | cmd -> all (mailfrom) context cmd

let authenticate text ?password context =
  begin
  match password with 
  | None -> plain_auth text
  | Some password ->
    authenticate_user ~b64:true text ~password ()
  end >>= fun (user,pswd,auth) ->
  if auth then (
    write context "235 2.7.0 Authentication successful" >>
    return (`Authenticated (user,pswd))
  ) else (
    write context "535 5.7.8 Authentication failed" >>
    return `Rset
  )

(* auth is done right after starttls *)
let rec authplain context =
  Log_.log `Debug "smtp: starting authplain\n";
  next ~isdata:true ~cur_state:`AuthPlain ~next_state:[`DataStream] context >>= function
  | `DataStream text ->
    authenticate text context
  | _ -> return `Rset (* will not get here, cause of DataStream *)

let rec authlogin user context =
  Log_.log `Debug "smtp starting authlogin\n";
  next ~isdata:true ~cur_state:`AuthLogin ~next_state:[`DataStream] context >>= function
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
  let (next_state,msg) =
    if context.config.auth_required then
      ([`Rset;`Ehlo],"530 5.7.0 Must issue a AUTH command first")
    else
      ([`MailFrom;`Ehlo;`Helo;`Noop;`Vrfy;`Rset],"503 5.5.1 Command out of sequence")
  in
  next ~cur_state:`Helo ~next_state ~msg context >>= function
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
  next ~cur_state:`AuthPlain ~next_state:[`Ehlo;`Helo;`AuthPlain;`AuthLogin]
      ~msg:"530 5.7.0 Must issue a AUTH command first" context >>= function
  | `AuthPlain text as cmd -> doauth context cmd
  | `AuthLogin as cmd -> doauth context cmd
  | cmd -> all (auth) context cmd

let rec ehlo context =
  Log_.log `Debug "smtp: starting ehlo\n";
  let (next_state,msg) = 
    if starttls_required context then
      ([`Ehlo;`Helo;`Rset;`Starttls],"530 5.7.0 Must issue a STARTTLS command first")
    else if auth_required context then
      ([`Ehlo;`Helo;`Rset;`AuthPlain;`AuthLogin],"530 5.7.0 Must issue a AUTH command first")
    else
      ([`MailFrom;`Ehlo;`Helo;`Noop;`Rset;`Vrfy;(*tmp*)`AuthPlain;`AuthLogin],"503")
  in
  next ~cur_state:`Ehlo ~next_state ~msg context >>= function
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
      ([`Ehlo;`Helo;`Rset;`Starttls],"530 5.7.0 Must issue a STARTTLS command first")
    else 
      ([`Ehlo;`Helo;`Rset;`Noop;`Vrfy],"503")
  in
  next ~cur_state:`Rset ~next_state ~msg context >>= function
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
    | `Authenticated up -> run (ehlo) {context with auth=Some up;grttype=`Ehlo;buff=Buffer.create 100}
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
  {config;auth=None;grttype=`Rset;io;from=("",None);rcpt=[];buff=Buffer.create 100}

let server_on_port addr port config =
  Log_.log `Info2 (Printf.sprintf "### start accepting on %s:%d\n" addr port);
  server (`Inet (addr, port)) config (fun sock r w ->
    greeting (mkcontext config sock r w))
    (fun ex -> Log_.log `Error (Printf.sprintf "### smtplet: exception %s\n"
    (Printexc.to_string ex)); return ())

let ports_str ports =
  List.fold_left (fun acc p -> acc ^ " " ^ (string_of_int p)) "" ports

let _ =
  Printexc.record_backtrace true;
  Lwt_main.run (
    Lwt_unix.handle_unix_error (fun () -> 
    (* temp, overwrite ssl/starttls with smtp_ssl/smtp_starttls *)
    let config = {srv_config with
      ssl=srv_config.smtp_ssl;starttls=srv_config.smtp_starttls} in
    Log_.log `Info1 (Printf.sprintf "### smtplet: started %s %s:%s:%b:%b\n" 
      (ImapTime.to_string (ImapTime.now()))
      config.smtp_addr (ports_str config.smtp_port) config.ssl config.starttls);
    Stun_maint.start config;
    Lwt_list.iter_p (fun port ->
      server_on_port config.smtp_addr port config) config.smtp_port
    ) ()
  )
