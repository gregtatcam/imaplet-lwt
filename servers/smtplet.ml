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
open Dates
open Smtplet_context

exception InvalidCmd
exception Valid

module MapStr = Map.Make(String)

(* interfaces on this server, need to reload if interfaces change, or
 * have a maintenance process to get it periodically *)
let lazy_interfaces = Lazy.from_fun (fun () -> Utils.get_interfaces ())

let lazy_hostname = Lazy.from_fun (fun () -> Lwt_unix.gethostname())

let _ = Log_.set_log "smtplet.log"

let send_stats = ref 0.

(* if user changes then the server has to reload, or can age it or 
 * have maintenance task
 *)
let auth_cache = ref MapStr.empty

let authenticate_user ?(b64=false) user ?password () =
  catch (fun () ->
    if MapStr.mem user !auth_cache then (
      return (MapStr.find user !auth_cache)
    ) else (
      Account.authenticate_user ~b64 user ?password () >>= fun (u,p,r,_) ->
      auth_cache := MapStr.add user (u,p,r) !auth_cache;
      return (u,p,r)
    )
  )
  (fun ex ->
    Log_.log `Error (Printf.sprintf "### authentication error: %s" (Printexc.to_string ex));
    return ("",None,false)
  )

let imap_write (ic,oc) _to pswd msg =
  Lwt_io.write oc (Printf.sprintf "a lappend %s%s INBOX {%d+}\r\n" _to pswd (String.length msg)) >>
  Lwt_io.write oc msg >>
  Lwt_io.read_line ic >>= fun _ ->
  return ()

(* write messages to IMAP on an async thread, to improve
 * context switch between read/write 
 *)
let imap_async_write (ic,oc) strm =
  let rec loop () =
    Lwt_stream.get strm >>= function
    | Some (_to,pswd,msg) ->
      imap_write (ic,oc) _to pswd msg >>
      loop ()
    | None -> return ()
  in
  loop ()

(* sending "special" local append on unix socket
 * should it be SSL? TBD 
 *)
let send_to_imap context =
  catch (fun () ->
  Log_.log `Info3 "smtp: sending to imap\n";
  let t = Unix.gettimeofday () in
  Log_.log `Debug (Printf.sprintf "smtp: time to send to imap %.04f\n" (t -.  !send_stats));
  send_stats := t;
  let (_from,domain) = context.from in
  let msg = 
    if context.config.stun_header = true then
      Stun_maint.cache_stun_records domain (Buffer.contents context.buff)
    else
      Buffer.contents context.buff
  in
  Log_.log `Info3 (Printf.sprintf "%s" (if String.length msg > 1000 then
    (String.sub msg 0 1000) else (msg)));
  let (_to,_,_) = List.hd context.rcpt in
  let up = context.auth in (* maybe don't need this TBD *)
  let pswd = 
    match up with
    | Some (_,pswd) -> (match pswd with |Some pswd -> " " ^ pswd|None->"")
    | None -> ""
  in
  (*context.push_strm (Some (_to,pswd,msg));*)
  imap_write context.lmtp _to pswd msg >>= fun () ->
  let t = Unix.gettimeofday () in
  Log_.log `Debug (Printf.sprintf "smtp: sent to imap %.04f\n" (t -.  !send_stats));
  send_stats := t;
  return true
  ) 
  (fun ex -> 
    Log_.log `Error (Printf.sprintf "### send to imap exception: %s\n" (Printexc.to_string ex)); 
    return false
  )

let write context msg = 
  Log_.log `Info1 (Printf.sprintf "<-- %s\n" msg);
  let (_,w,_) = context.io in
  Lwt_io.write w msg >>
  Lwt_io.write w "\r\n" 

let write_return context msg log res =
  log res;
  write context msg >>
  return res

let log_return log res =
  log res;
  return res

let read context =
  let (r,_,_) = context.io in
  catch (fun () ->
    Utils.with_timeout_cancel (int_of_float context.config.smtp_idle_max)
      (fun () -> Lwt_io.read_line_opt r)
  ) (function
    | Canceled ->
      Lazy.force lazy_hostname >>= fun host ->
      write context ("421 4.4.2 " ^ host ^ " Error: timeout exceeded") >>
      return None
    | ex -> Log_.log `Error 
      (Printf.sprintf "smtp:read exception %s\n" (Printexc.to_string ex));
      return None)

let buffer_ends_crlf buffer =
  let len = Buffer.length buffer in
  if len < 2 then
    false
  else
    Buffer.nth buffer (len - 2) = '\r' && Buffer.nth buffer (len - 1) = '\n'

let syntx_helo log next_state ~msg str context =
  if List.exists (fun s -> s = `Helo) next_state = false then (
    write_return context msg log `NextOutOfSeq
  ) else if Regex.match_regex ~regx:"^[ \t]+\\([^ \t]\\)+$" str then (
    write_return context "250 OK" log `Helo
  ) else (
    write_return context "501 5.5.2 Syntax: HELO hostname" log `Next
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
     * required if at all, since the relayed message does not have
     * the password *)
    (*context.auth = None *) false
  else
    false

let syntx_ehlo log next_state ~msg str context =
  if List.exists (fun s -> s = `Ehlo) next_state = false then (
    write_return context msg log `NextOutOfSeq
  ) else if Regex.match_regex ~regx:"^[ \t]+\\([^ \t]+\\)$" str then (
    Lazy.force lazy_hostname >>= fun host ->
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
    log_return log `Ehlo
  ) else (
    write_return context "501 5.5.2 Syntax: EHLO hostname" log `Next
  )

let syntx_rset log next_state ~msg context =
  if List.exists (fun s -> s = `Rset) next_state = false then (
    write_return context msg log `NextOutOfSeq
  ) else (
    write_return context "250 OK" log `Rset
  )

let syntx_noop log next_state ~msg context =
  if List.exists (fun s -> s = `Noop) next_state = false then (
    write_return context msg log `NextOutOfSeq
  ) else (
    write_return context "250 OK" log `Next
  )

let syntx_vrfy log next_state ~msg str context =
  if List.exists (fun s -> s = `Vrfy) next_state = false then (
    write_return context msg log `NextOutOfSeq
  ) else if Regex.match_regex ~regx:"^[ \t]+<?\\([^ <>\t]+\\)>?$" str then (
    let user = Str.matched_group 1 str in
    authenticate_user user () >>= fun (_,_,auth) ->
    if auth then (
      write_return context ("252 " ^ user) log `Next
    ) else (
      write_return context 
        ("550 5.7.8 " ^ user ^ "Recipient address rejected: User unknown in local recipient table") log `Next
    )
  ) else (
    write_return context "501 5.5.2 Bad recipient address syntax" log `Next
  ) 

let syntx_quit log context =
  write_return context "250 OK" log `Quit

let syntx_from log next_state ~msg cmd context =
  if List.exists (fun s -> s = `MailFrom) next_state = false then (
    write_return context msg log `NextOutOfSeq
  ) else if Regex.match_regex ~case:false ~regx:"^[ \t]+FROM:[ \t]*<?\\([^ <>@\t]+\\)\\(@\\([^>]*\\)\\)?" cmd then (
    let user = Str.matched_group 1 cmd in
    let domain = try Some (Str.matched_group 3 cmd) with Not_found -> None in
    write_return context "250 OK" log (`MailFrom (user,domain))
  ) else (
    write_return context "501 5.5.2 Syntax: MAIL FROM:<address>" log `Next
  )

(* is this the master domain 
 *)
let is_master_domain domain context =
  match context.config.master with
  | Some master 
    when master <> "localhost" && master <> "127.0.0.1" && master = domain -> true
  | _ -> false

(* is the domain in the server's domain
 *)
let in_my_domain context domain =
  let interface = context.config.smtp_addr in (* interface listening on *)
  let mydomain = context.config.domain in (* server's domain *)
  Lazy.force lazy_interfaces >>= fun my_ips ->
  Log_.log `Info2 
    (Printf.sprintf "### detecting send to domain %s on interface %s, my domain
    %s, configured domains %s\n" 
    domain interface mydomain context.config.domain) ;
  begin
  let dexists domains domain = 
    List.exists (fun d -> d = domain) (Str.split (Str.regexp ";") domains) in
  if dexists context.config.domain domain || is_master_domain domain context then (
    Log_.log `Debug (Printf.sprintf "### domain exists or is master\n%!");
    return my_ips (* it's my domain *)
  ) else if (try let _ = Unix.inet_addr_of_string domain in true with _ -> false) then (
    Log_.log `Debug (Printf.sprintf "### domain is ip\n%!");
    return [domain] (* already ip *)
  ) else (
    Log_.log `Debug (Printf.sprintf "### resolving domain via dns\n%!");
    Imaplet_dns.gethostbyname ?config:(context.config.resolve) domain >>= fun ips ->
    if List.length ips = 0 then
      Utils.gethostbyname domain (* could be local host *)
    else
      return ips
  )
  end >>= fun domain_ips ->
  (* find intersection of domain ip's with this server ip's *)
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
    (* listening on any interface or specific interface *)
    if interface = "0.0.0.0" || interface = ip then
      return `Yes
    else (* server is not listening on this interface *)
      return `NoInterface

(* authenticate user in the domain *)
let authenticate_user_domain user = function
  | None -> authenticate_user user ()
  | Some domain ->
    let fqn = user ^ "@" ^ domain in
    authenticate_user fqn () >>= fun (_,p,auth) ->
    if auth then 
      return (fqn,p,auth)
    else (
      authenticate_user user () >>= fun (_,p,auth) ->
      return (user,p,auth)
    )

(* restrict relay to domain/users specified in the restriction file *)
let restrict_relay context =
  let (user,domain) = context.from in
  match context.config.relayfrom with
  | Some users ->
    Utils.lines_of_file ~g:(function Valid -> return true|ex -> raise ex) users ~init:false ~f:(fun line res ->
      (* match user?@domain *)
      if Regex.match_regex ~regx:"^\\([^@]+\\)?@\\(.+\\)$" line then (
        let ruser = try Str.matched_group 1 line with Not_found -> "" in
        let rdomain = Str.matched_group 2 line in
        let res = 
          match domain with
          | None -> false (* from user doesn't have domain *)
          | Some domain -> 
            (* if no user restriction then match domain, otherwise domain and user *)
            domain = rdomain && (ruser = "" || ruser = user) 
        in
        if res then
          raise Valid
        else
          return res
      ) else (
        if user = line then
          raise Valid
        else
          return false
      )
    ) 
  | None -> return true

(* from user must have the account *)
let valid_from context =
  let (user,domain) = context.from in
  begin
  authenticate_user_domain user domain >>= fun (_,_,auth) ->
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
  end >>= fun res ->
  if res then
    restrict_relay context 
  else
    return res

let syntx_rcpt log next_state ~msg cmd context =
  let t = Unix.gettimeofday () in
  if List.exists (fun s -> s = `RcptTo) next_state = false then (
    write_return context msg log `NextOutOfSeq
  ) else if Regex.match_regex ~case:false ~regx:"^[ \t]+TO:[ \t]*<?\\([^ <>@\t]+\\)@\\([^>]*\\)" cmd then (
    let user = Str.matched_group 1 cmd in
    let domain =  Str.matched_group 2 cmd in
    in_my_domain context domain >>= fun res ->
    if res = `NoInterface then ( (* sent to wrong interface *)
      write_return context "550 5.1.2 : Not accepting on this network interface" log `Next
    ) else if res = `Yes then (
      authenticate_user_domain user (Some domain) >>= fun (fqn,_,auth) ->
      if auth then (
        Log_.log `Debug (Printf.sprintf "rcpt to time %.04f\n" (Unix.gettimeofday() -. t));
        write_return context "250 OK" log (`RcptTo (fqn,domain,`None))
      ) else (
        write_return context 
          "550 5.7.8 : Recipient address rejected: User unknown in local recipient table" log `Next
      )
    (* have to relay, only allow authenticated users to relay *)
    ) else if context.config.relay_authreq && context.auth = None then (
      write_return context "550 5.7.1 : Authentication required" log `Next
    ) else (
      begin
      if context.auth <> None then
        restrict_relay context
      else
        valid_from context
      end >>= fun valid ->
      if valid then (
        (* route optimization should go in here -
         * if the destination is public then should send directly to it, etc TBD *)
        (* if have master then relay via the master *)
        let relay_domain =
          match context.config.master with
          | Some master when master <> "localhost" && master <> "127.0.0.1" -> 
            Log_.log `Info1 
              (Printf.sprintf "### scheduling to relay to domain %s via master %s\n" domain master);
            master
          | _ -> 
            Log_.log `Info1 (Printf.sprintf "### scheduling to relay to domain %s\n" domain);
            domain
        in
        Imaplet_dns.resolve ?config:(context.config.resolve) relay_domain >>= fun mx_rr ->
        (* might be relaying directly to another device in the same network,
         * need a way to figure it out?
         *)
        if List.length mx_rr > 0 then (
          write_return context "250 OK" log (`RcptTo (user,domain,`MXRelay mx_rr))
        ) else if (try let _ = Unix.inet_addr_of_string relay_domain in true with _ -> false) then (
          (* temp work around for direct send - if the address is ip then use
           * it, need to make a more general case, look at the headers, check if
           * STUN mapped address in the header (additional header X-?) matches
           * this STUN mapped address *)
          write_return context "250 OK" log (`RcptTo (user,domain, `DirectRelay (relay_domain,[25;587;2587])))
        ) else (
          (* local hostname *)
          Utils.gethostbyname relay_domain >>= fun hosts ->
          if List.length hosts > 0 then (
            log_return log (`RcptTo (user,domain, `DirectRelay (List.hd hosts,[25;587;2587])))
          ) else (
            write_return context "550 5.1.2 : Invalid domain" log `Next
          )
        )
      ) else (
        write_return context 
          "550 5.7.8 : From address rejected: User unknown in local recipient table or invalid domain" 
          log `Next
      )
    )
  ) else (
    write_return context "501 5.5.2 Syntax: RCPT TO:<address>" log `Next
  )

let syntx_data log next_state ~msg cmd context =
  if List.exists (fun s -> s = `Data) next_state = false then (
    write_return context msg log `NextOutOfSeq
  ) else if Regex.match_regex ~regx:"^[ \t]*$" cmd then (
    let t = Unix.gettimeofday () in
    Log_.log `Debug (Printf.sprintf "smtp: start data, time lapse %.04f\n" (t -.  !send_stats));
    send_stats := t;
    write_return context "354 End data with <CR><LF>.<CR><LF>" log `Data
  ) else (
    write_return context "501 5.5.2 Syntax: DATA" log `Next
  )

let syntx_starttls log next_state ~msg cmd context =
  if List.exists (fun s -> s = `Starttls) next_state = false then (
    write_return context msg log `NextOutOfSeq
  ) else if Regex.match_regex ~regx:"^[ \t]*$" cmd = false then (
    write_return context "501 5.5.2 Syntax error" log `Next
  ) else (
    let (_,_,s) = context.io in
    match s with
    | `Reg sock -> 
      write_return context "220 ready to start TLS" log (`Starttls sock)
    | _ ->
      write_return context "454 4.5.0 TLS not available due to local problem" log `Next
  )

let syntx_auth log next_state ~msg cmd context =
  if List.exists (fun s -> s = `AuthPlain || s = `AuthLogin) next_state = false then (
    write_return context msg log `NextOutOfSeq
  ) else if Regex.match_regex ~case:false 
    ~regx:"^[ \t]+PLAIN[ \t]+\\([^ \t]+\\)$" cmd then (
    log_return log (`AuthPlain (Str.matched_group 1 cmd))
  ) else if Regex.match_regex ~case:false ~regx:"^[ \t]+PLAIN[ \t]*$" cmd then (
    (* request the text *)
    write_return context "334 " log (`AuthPlain "")
  ) else if Regex.match_regex ~case:false ~regx:"^[ \t]+LOGIN[ \t]*$" cmd then (
    (* 334 Username *)
    write_return context "334 VXNlcm5hbWU6" log `AuthLogin
  ) else (
    write_return context "501 5.5.2 Syntax error" log `NextAuthError
  )

let dolog str cur_state next_state =
  Log_.log `Info3 (Printf.sprintf "--> %s\n" 
    begin
    match next_state with
    | `AuthPlain _ -> "AUTH PLAIN ..."
    | `AuthLogin -> "AUTH LOGIN ..."
    | `NextAuthError -> "AUTH error ..."
    | `NextOutOfSeq when (String.lowercase (String.sub str 0 4)) = "auth" -> "AUTH out of seq ..."
    | `DataStream _ when cur_state = `AuthPlain || cur_state = `AuthLogin -> "AUTH data ..."
    | _ -> str
    end)

let next ?(isdata=false) ?(msg="503 5.5.1 Command out of sequence") ~cur_state ~next_state context =
  read context >>= function
  | None -> Log_.log `Debug "smtp: client terminated\n";return `Quit
  | Some str -> 
  let log = dolog str cur_state in
  let domatch ?(tmpl="\\(.*\\)$") rx =
    Regex.match_regex ~case:false ~regx:("^[ \t]*" ^ rx ^ tmpl) str in
  let get () = try Str.matched_group 1 str with _ -> "" in
  if isdata then (
    log_return log (`DataStream str)
  ) else if domatch ~tmpl:"\\([ \t]+[^ \t]+?\\)?$" "HELO" then (
    syntx_helo log next_state ~msg (get ()) context
  ) else if domatch ~tmpl:"\\([ \t]+[^ \t]+?\\)?$" "EHLO" then (
    syntx_ehlo log next_state ~msg (get ()) context
  ) else if domatch ~tmpl:"\\([ \t]+[^ \t]+?.*\\)?$" "MAIL" then (
    syntx_from log next_state ~msg (get ()) context
  ) else if domatch ~tmpl:"\\([ \t]+[^ \t]+?.*\\)?$" "RCPT" then (
    syntx_rcpt log next_state ~msg (get ()) context
  ) else if domatch ~tmpl:"\\([ \t]*.*\\)$" "DATA" then (
    syntx_data log next_state ~msg (get()) context
  ) else if domatch ~tmpl:"\\([ \t]+[^ \t]+?\\)?$" "NOOP" then (
    syntx_noop log next_state ~msg context
  ) else if domatch ~tmpl:"\\([ \t]+[^ \t]+?\\)?$" "RSET" then (
    syntx_rset log next_state ~msg context
  ) else if domatch ~tmpl:"\\([ \t]+[^ \t]+?\\)?$" "STARTTLS" then (
    syntx_starttls log next_state ~msg (get ()) context
  ) else if domatch ~tmpl:"\\([ \t]+[^ \t]+?\\)?$" "QUIT" then (
    syntx_quit log context
  ) else if domatch ~tmpl:"\\([ \t]+[^ \t]+?\\)?$" "VRFY" then (
    syntx_vrfy log next_state ~msg (get ()) context
  ) else if domatch ~tmpl:"\\([ \t]+.*]+?\\)?$" "AUTH" then (
    syntx_auth log next_state ~msg (get ()) context
  ) else (
    write_return context "502 5.5.1 Error: command not recognized" log `Next
  )

let all state context = function
  | `Quit -> return `Quit
  | `Rset -> return `Rset
  | `Helo -> return `Helo
  | `Ehlo -> return `Ehlo
  | `Next -> return (`State (state,context))
  | _ -> return (`State (state,context))

let spoof_master content context =
  match context.config.master with
  | Some master when master <> "localhost" && master <> "127.0.0.1" ->
    Log_.log `Debug "### spoofing master\n";
    if Str.string_match (Str.regexp_case_fold "^from:") content 0 then
      Str.replace_first (Str.regexp "[^@]+>") (master ^ ">") content
    else
      content
  | _ -> content

let rec datastream context =
  Log_.log `Debug "smtp: starting datastream\n";
  next ~isdata:true ~cur_state:`DataStream ~next_state:[`DataStream] context >>= function
  | `DataStream str ->
    let str = spoof_master str context in
    if buffer_ends_crlf context.buff && str = "." then (
      let t = Unix.gettimeofday () in
      Log_.log `Debug 
        (Printf.sprintf "smtp: stats report, data collection %.04f\n" (t -. !send_stats));
      send_stats := t;
      (* send relayed on another thread *)
      async (fun () ->
        Lwt_list.iter_p (fun rcpt ->
          let context = {context with rcpt = [rcpt]} in
          let (_,_,relay_rec) = rcpt in
          if relay_rec = `None then (
            return ()
          ) else (
            Smtplet_relay.relay context.config.stun_header context 
              (fun context -> send_to_imap context >>= fun _ -> return())
          )
        ) context.rcpt
      );
      (* serialize send to the local accounts *)
      Lwt_list.fold_left_s (fun acc rcpt ->
        let (_,_,relay_rec) = rcpt in
        if relay_rec = `None then (
          let context = {context with rcpt = [rcpt]} in
          send_to_imap context >>= fun res ->
          return (acc && res)
        ) else (
          return acc
        )
      ) true context.rcpt >>= fun res ->
      write context (if res then "250 OK" else "554 5.5.0 Transaction failed") >>
      return `Rset
    ) else (
      Buffer.add_string context.buff str;
      Buffer.add_string context.buff "\r\n";
      return (`State (datastream, context))
    )
  (* anything is data until done, would not get to this state *)
  | _ -> return `Rset

let rec rcptto context =
  Log_.log `Debug "smtp: starting rcptto\n";
  next ~cur_state:`RcptTo ~next_state:[`RcptTo;`Data;`Vrfy;`Noop;`Helo;`Ehlo;`Rset] context >>= function
  | `RcptTo r -> return (`State (rcptto, {context with rcpt = r :: context.rcpt}))
  | `Data -> 
    Buffer.clear context.buff;
    return (`State (datastream,context))
  | cmd -> all (rcptto) context cmd

let rec mailfrom context =
  Log_.log `Debug "smtp: starting mailfrom\n";
  next ~cur_state:`MailFrom ~next_state:[`RcptTo;`Vrfy;`Noop;`Helo;`Ehlo;`Rset] context >>= function 
  | `RcptTo r -> return (`State (rcptto, {context with rcpt = [r]}))
  | cmd -> all (mailfrom) context cmd

let authenticate text ?password context =
  begin
  match password with 
  | None -> Account.plain_auth text >>= fun (user,pswd,auth,_) ->
    return (user,pswd,auth)
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
    Log_.log `Info3 (Printf.sprintf "### REMOVE THIS authplain %s\n" text); 
    authenticate text context
  | _ -> return `Rset (* will not get here, cause of DataStream *)

let rec authlogin user context =
  Log_.log `Debug "smtp starting authlogin\n";
  next ~isdata:true ~cur_state:`AuthLogin ~next_state:[`DataStream] context >>= function
  | `DataStream text -> 
    Log_.log `Info3 (Printf.sprintf "### REMOVE THIS authlogin %s\n" text); 
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
    | `Ehlo -> 
      Buffer.clear context.buff;
      run (ehlo) {context with grttype=`Ehlo;}
    | `Authenticated up -> 
      Buffer.clear context.buff;
      run (ehlo) {context with auth=Some up;grttype=`Ehlo;}
    | `Helo -> 
      Buffer.clear context.buff;
      run (helo) {context with grttype=`Helo;}
    | `Rset -> 
      Buffer.clear context.buff;
      if context.grttype = `Ehlo then
        run (ehlo) context
      else
        run (helo) context
    | `State (state,context) -> run (state) context
    | _ -> write context "503 5.5.1 Command out of sequence" >> run (state) context
  in
  run (start) context

let mkcontext config sock inchan outchan push_strm lmtp =
  let io =
    match sock with
    | None -> (inchan,outchan,`Ssl)
    | Some sock -> (inchan,outchan,`Reg sock)
  in
  {config;auth=None;grttype=`Rset;io;from=("",None);rcpt=[];
    buff=Buffer.create 1_000;lmtp;push_strm}

let server_on_port addr port push_strm lmtp config =
  Log_.log `Info2 (Printf.sprintf "### start accepting on %s:%d\n" addr port);
  server (`Inet (addr, port)) config (fun sock r w ->
    greeting (mkcontext config sock r w push_strm lmtp))
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
    (* open connection to the IMAP server *)
    Lwt_io.open_connection 
      (Unix.ADDR_UNIX (Filename.concat Install.data_path "sock/smtp")) >>= fun lmtp ->
    let (strm,push_strm) = Lwt_stream.create () in
    (* start async thread for IMAP write *)
    async(fun() -> imap_async_write lmtp strm);
    Lwt_list.iter_p (fun port ->
      server_on_port config.smtp_addr port push_strm lmtp config) config.smtp_port
    ) ()
  )
