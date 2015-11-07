open Lwt
open X509.Encoding.Pem
open Imaplet
open Commands
open Parsemail

module MapStr = Map.Make(String)

exception InvalidCommand
exception Failed
exception LoginFailed

let echoterm = ref false

let echo_on on =
  Lwt_unix.system (Printf.sprintf "stty %secho" (if on then "" else "-")) >>=
    fun _ -> return ()

let gmail host = if host = "imap.gmail.com" then true else false

let get_arch str =
  let re = Re_posix.compile_pat "^(mbox|maildir|imap|imap-dld):(.+)$" in
  let subs = Re.exec re str in
  let arch = Re.get subs 1 in
  let args = Re.get subs 2 in
  match arch with
  | "mbox" -> `Mbox args
  | "maildir" -> `Maildir args
  | "imap" | "imap-dld" ->
    let re = Re_posix.compile_pat "^([^:]+)(:([0-9]+))?$" in
    let subs = Re.exec re args in
    let host = 
      match Re.get subs 1 with
      | "gmail" -> "imap.gmail.com"
      | "yahoo" -> "imap.mail.yahoo.com"
      | "outlook" -> "imap-mail.outlook.com"
      | "aol" -> "imap.aol.com"
      | "icloud" -> "imap.mail.me.com"
      | "mail" -> "imap.mail.com"
      | "hermes" -> "imap.hermes.cam.ac.uk"
      | other -> other
    in
    let port = try Some (int_of_string (Re.get subs 3)) with Not_found -> None in
    if arch = "imap" then
      `Imap (host,port)
    else
      `ImapDld (host,port)
  | _ -> raise InvalidCommand

let opt_val = function
  | None -> raise InvalidCommand
  | Some v -> v

let rec args i archive echo =
  if i >= Array.length Sys.argv then
    archive,echo
  else
    match Sys.argv.(i) with 
    | "-archive" -> args (i+2) (Some (get_arch Sys.argv.(i+1))) echo 
    | "-echo" -> args (i+1) archive true 
    | _ -> raise InvalidCommand

let commands f =
  try 
    let archive,echo = args 1 None false in
    f (opt_val archive) echo
  with 
    | InvalidCommand ->
    Printf.printf "usage: email_stats -archive [mbox:file|imap:server:port:user:password]\n%!"
    | _ -> ()


let email_addr = ref MapStr.empty
let email_addr_cnt = ref 0
let attachments = ref MapStr.empty
let attachments_cnt = ref 0
let mailboxes = ref MapStr.empty
let mailboxes_cnt = ref 0
let subject = ref MapStr.empty
let subject_cnt = ref 0
let messageid = ref MapStr.empty
let messageid_cnt = ref 0

let get_unique key cnt map =
  if key = "" then
    ""
  else (
  try 
    string_of_int (MapStr.find key !map)
  with Not_found ->
    cnt := !cnt + 1;
    map := MapStr.add key !cnt !map;
    string_of_int !cnt
  )

let get_addr key =
  get_unique key email_addr_cnt email_addr

let get_attach key =
  get_unique key attachments_cnt attachments 

let get_mailbox key =
  let key = Re.replace_string (Re_posix.compile_pat "^\"|\"$") ~by:"" key in
  get_unique key mailboxes_cnt mailboxes 

let get_subject key =
  get_unique key subject_cnt subject

let get_messageid key =
  get_unique key messageid_cnt messageid

let headers_to_map headers =
  List.fold_left (fun map (n,v) ->
    let n = String.lowercase n in
    if n = "from" || n = "to" || n = "cc" || n = "subject" || n = "object-id" || 
        n = "in-reply-to" || n = "x-gmail-labels" || n = "content-type" || 
        n = "date" || n = "message-id" then
      MapStr.add n (String.trim v) map
    else
      map
  ) MapStr.empty headers

let get_header_l headers name =
  try
    let n,v = List.find (fun (n,v) -> String.lowercase n = name) headers in
    v
  with Not_found -> ""

let get_header_m headers name =
  try 
    MapStr.find name headers
  with Not_found -> ""


let re_addr = Re_posix.compile_pat "([^<@]+@[^>]+)"

let get_email_addr addr =
  if addr <> "" then (
    try 
      let subs = Re.exec re_addr addr in
      Re.get subs 1
    with Not_found -> ""
  ) else 
    ""

let _from headers =
  let h = get_header_m headers "from" in
  get_addr (get_email_addr h)

let _to headers =
  let h = get_header_m headers "to" in
  let hl = Re.split (Re_posix.compile_pat ",") h in
  String.concat "," (List.map (fun h -> get_addr (get_email_addr h)) hl)

let _cc headers =
  let h = get_header_m headers "cc" in
  let hl = Re.split (Re_posix.compile_pat ",") h in
  String.concat "," (List.map (fun h -> get_addr (get_email_addr h)) hl)

let _gmail_labels headers =
  let h = get_header_m headers "x-gmail-labels" in
  if h = "" then
    get_mailbox "inbox"
  else (
    let l = Re.split (Re_posix.compile_pat ",") h in
    String.concat "," (List.map (fun h ->
      if Re.execp (Re_posix.compile_pat "[[]Gmail[]]") h = false then (
        let m = Re.split (Re_posix.compile_pat "/") h in
        if List.length m > 1 then (
          String.concat "/" (List.map (fun m ->
            get_mailbox (String.lowercase m)
          ) m)
        ) else (
          get_mailbox (String.lowercase h)
        )
      ) else
        get_mailbox h
    ) l)
  )

let _messageid headers =
  let h = get_header_m headers "message-id" in
  get_messageid h

let _inreplyto headers =
  let h = get_header_m headers "in-reply-to" in
  let l = Re.split (Re_posix.compile_pat "[ \n\r]+]") h in
  String.concat "," (List.map (fun h -> get_messageid h) l)

let re_subject = Re_posix.compile_pat ~opts:[`ICase] "(re|fw|fwd): "
let _subject headers =
  let h = get_header_m headers "subject" in
  let l = Re.split re_subject h in
  let len = List.length l in
  if len = 0 then
    ""
  else (
  let s = List.nth l (len - 1) in
  let s = get_subject s in
  if len > 1 then
    "re/fw: " ^ s
  else
    s
  )

let get_hdr_attrs headers =
  List.fold_left (fun (attach,rfc822) (n,v) ->
    if Re.execp (Re_posix.compile_pat ~opts:[`ICase] "Content-Type") n then (
      let rfc822 =
        if Re.execp (Re_posix.compile_pat ~opts:[`ICase] "message/rfc822") v then
          true
        else
          false
      in
      let attach =
        if Re.execp (Re_posix.compile_pat ~opts:[`ICase] "image|application|audio|video") v then
          true
        else
          attach
      in
      attach,rfc822
    ) else
      attach,rfc822
  ) (false,false) headers

let email_raw_content email =
  match (Email.raw_content email) with
  | Some rc -> Octet_stream.to_string rc
  | None -> ""

let headers_to_string headers =
  String_monoid.to_string (Header.to_string_monoid headers)

let len_compressed content = 
  String.length (Imap_crypto.do_compress ~header:true content)

let rec walk email id part multipart =
  let headers = Header.to_list (Email.header email) in
  let headers_s = headers_to_string (Email.header email) in
  let attach,rfc822 = get_hdr_attrs headers in
  Printf.printf "part: %d\n%!" part;
  (*Printf.printf "headers:\n%s\n%!" headers_s;*)
  Printf.printf "headers: %d %d %d\n%!" (List.length headers) (String.length headers_s) 
    (len_compressed headers_s);
  match Email.content email with
  | `Data _ ->
    let content = email_raw_content email in
    (*Printf.printf "content:\n%s\n%!" content;*)
    if multipart && rfc822 then (
      Printf.printf "start rfc822: %d\n%!" id;
      let email = Email.of_string content in
      walk email (id+1) 0 multipart;
      Printf.printf "end rfc822: %d\n%!" id;
    ) else if attach then (
      let sha = Imap_crypto.get_hash ~hash:`Sha1 content in
      Printf.printf "attachment: %s %d %d\n%!" (get_attach sha) 
        (String.length content) (len_compressed content)
    ) else (
      Printf.printf "body: %d %d\n%!" (String.length content) (len_compressed content)
    )
  | `Message _ -> assert (false)
  | `Multipart elist ->
    Printf.printf "start multipart %d %d:\n%!" id (List.length elist);
    let _ = List.fold_left (fun part email ->
      walk email (id+1) part true;
      part + 1
    ) 0 elist in
    Printf.printf "end multipart %d\n%!" id

let mailboxes_init () =
  let _ = get_mailbox "inbox" in
  let _ = get_mailbox "sent" in
  let _ = get_mailbox "sent messages" in
  let _ = get_mailbox "\"[Gmail]/Sent Mail\"" in
  let _ = get_mailbox "trash" in
  let _ = get_mailbox "\"[Gmail]/Trash\"" in
  let _ = get_mailbox "junk" in
  let _ = get_mailbox "deleted" in
  let _ = get_mailbox "deleted messages" in
  let _ = get_mailbox "spam" in
  let _ = get_mailbox "\"[Gmail]/Spam\"" in
  let _ = get_mailbox "\"[Gmail]/All Mail\"" in
  let _ = get_mailbox "\"[Gmail]/Important\"" in
  let _ = get_mailbox "drafts" in
  let _ = get_mailbox "\"[Gmail]/Drafts\"" in ()

let ok res =
  let re = Re_posix.compile_pat ~opts:[`ICase] ("^[^\\* ]+ ok( .*)?$") in
  Re.execp re res

let bad res =
  let re = Re_posix.compile_pat ~opts:[`ICase] ("^[^\\* ]+ bad( .*)?$") in
  Re.execp re res

let no res =
  let re = Re_posix.compile_pat ~opts:[`ICase] ("^[^\\* ]+ no( .*)?$") in
  Re.execp re res

let ok_ex res =
  if bad res || no res then
    raise Failed;
  ok res

let exists res =
  let re = Re_posix.compile_pat ~opts:[`ICase] ("^\\* ([0-9]+) exists") in
  if Re.execp re res then
    let subs = Re.exec re res in
    (true,int_of_string (Re.get subs 1))
  else
    (false,0)

let write oc msg =
  if !echoterm then
    Printf.fprintf stderr "%s%!" msg;
  Lwt_io.write oc msg

let read_line ?(force=false) ic =
  Lwt_io.read_line ic >>= fun l ->
  if force || !echoterm then
    Printf.fprintf stderr "%s\n%!" l;
  return l

let while_not_ok ic =
  let rec read () =
    read_line ic >>= fun res ->
    if ok res then
      return ()
    else if bad res || no res then
      raise Failed
    else 
      read ()
  in
  read ()

let tag = ref 0

let get_tag () =
  tag := !tag + 1;
  Printf.sprintf "a%04d" !tag

let capability ic oc =
  write oc (Printf.sprintf "%s capability\r\n" (get_tag())) >>= fun () ->
  while_not_ok ic

let login user pswd ic oc =
  catch (fun() ->
    write oc (Printf.sprintf "%s login %s %s\r\n" (get_tag()) user pswd) >>= fun () ->
    while_not_ok ic
  ) (fun _ -> raise LoginFailed)

let logout oc =
  write oc (Printf.sprintf "%s logout\r\n" (get_tag()))

let status ic oc mailbox =
  write oc (Printf.sprintf "%s status %s (messages)\r\n" (get_tag()) mailbox) >>= fun () ->
  read_line ic >>= fun res ->
  let msgs = 
    try
      let re = Re_posix.compile_pat ~opts:[`ICase] "messages ([0-9]+)\\)$" in
      let subs = Re.exec re res in
      Re.get subs 1
    with Not_found -> "-" 
  in
  while_not_ok ic >>= fun () ->
  return msgs

(*
   * LIST (\HasNoChildren) "/" "Bulk Mail"
   * LIST (\HasChildren) "/" "Cambridge"
   * LIST (\HasNoChildren) "/" "Cambridge/Entertainment"
 *)
let list host ic oc = 
  write oc (Printf.sprintf "%s list \"\" *\r\n" (get_tag())) >>= fun () ->
    let re = Re_posix.compile_pat ~opts:[`ICase] "^\\* list \\(([^)]+)\\) [^ ]+ ((\"[^\"]+\")|([^\t ]+))$" in
  Printf.fprintf stderr "mailboxes to process:\n%!";
  let rec read l =
    read_line ic >>= fun res ->
    if ok res then
      return l
    else (
      let subs = Re.exec re res in
      let mailbox = Re.get subs 2 in
      let attrs = Re.split (Re_posix.compile_pat " ") (Re.get subs 1) in
      if List.exists (fun attr -> (String.lowercase attr) = "\\noselect") attrs ||
          mailbox = "\"[Gmail]/All Mail\"" || mailbox = "\"[Gmail]/Important\"" || 
          mailbox = "\"[Gmail]/Starred\"" then
        read l
      else (
        read (mailbox :: l)
      )
    )
  in 
  read (if gmail host then ["\"[Gmail]/All Mail\"";"\"[Gmail]/Important\""] else []) >>= fun l ->
  let l = ["\"Cambridge\""] in
  let total = ref 0 in
  Lwt_list.map_s (fun m ->
    status ic oc m >>= fun msgs ->
    Printf.fprintf stderr "%s, %s messages\n%!" m msgs;
    let msgs = int_of_string msgs in
    total := !total + msgs;
    return (msgs,m)
  ) l >>= fun l ->
  Printf.fprintf stderr "Total messages: %d\n%!" !total;
  return l

let select ic oc mailbox =
  write oc (Printf.sprintf "%s select %s\r\n" (get_tag()) mailbox) >>= fun () ->
  let rec read msgs =
    read_line ic >>= fun res ->
    if ok res then
      return msgs
    else (
      let ex,m = exists res in
      let msgs = if ex then m else msgs in
      read msgs
    )
  in 
  read 0

let get_part ic =
  let re_fetch = Re_posix.compile_pat ~opts:[`ICase] 
    "^\\* ([0-9]+) fetch [^}]+[{]([0-9]+)[}]$" in (* * 5 FETCH (BODY[] {1208} *)
  read_line ic >>= fun res ->
  let subs = Re.exec re_fetch res in
  let count = int_of_string (Re.get subs 2) in
  let msg = String.create count in
  Lwt_io.read_into_exactly ic msg 0 count >>= fun () ->
  read_line ic >>= fun res ->
  if res <> ")" then raise Failed;
  read_line ic >>= fun res ->
  if ok res = false then raise Failed;
  return msg

let fetch_part ic oc i part =
  write oc (Printf.sprintf "%s fetch %d body[%s]\r\n" (get_tag()) i part) >>= fun () ->
  get_part ic

let fetch_unique ic oc mailbox cnt f =
  let rec _fetch i =
    if i > cnt then 
      return ()
    else (
      fetch_part ic oc i "header" >>= fun headers ->
      let re = Re_posix.compile_pat ~opts:[`ICase] "message-id: ([^ \r\n]+)" in
      let msgid =
        try
          let subs = (Re.exec re headers) in
          Re.get subs 1
        with Not_found -> ""
      in
      if msgid <> "" && MapStr.mem msgid !messageid then (
        Printf.fprintf stderr "duplicate message\n%!";
        _fetch (i + 1)
      ) else (
        let _ = get_messageid msgid in
        fetch_part ic oc i "text" >>= fun body ->
        let buffer = Buffer.create ((String.length headers) + (String.length body) + 2) in
        Buffer.add_string buffer headers;
        Buffer.add_string buffer "\r\n";
        Buffer.add_string buffer body;
        f i mailbox (Buffer.contents buffer) >>= fun () ->
        _fetch (i + 1)
      )
    )
  in
  _fetch 1

let fetch ic oc mailbox f =
  let re_fetch = Re_posix.compile_pat ~opts:[`ICase] 
    "^\\* ([0-9]+) fetch [^}]+[{]([0-9]+)[}]$" in (* * 5 FETCH (BODY[] {1208} *)
  write oc (Printf.sprintf "%s fetch 1:* body[]\r\n" (get_tag())) >>= fun () ->
  let rec read () =
    read_line ic >>= fun res ->
    if ok_ex res then (
      return () 
    ) else if Re.execp re_fetch res then (
      let subs = Re.exec re_fetch res in
      let id = int_of_string (Re.get subs 1) in
      let count = int_of_string (Re.get subs 2) in
      let msg = String.create count in
      Lwt_io.read_into_exactly ic msg 0 count >>= fun () ->
      f id mailbox msg >>= fun () ->
      read ()
    ) else  (
      read ()
    )
  in
  read ()

let start_plain ip port f =
  let open Socket_utils in
  client_send (`Inet (ip,port)) (fun res sock ic oc -> 
    Lwt_io.read_line ic >>= fun _ -> (* capabilities *)
    f ic oc
  ) ()

let start_ssl ip port f =
  let open Socket_utils in
  client_send (`Inet (ip,port)) (fun res sock ic oc -> 
    Lwt_io.read_line ic >>= fun _ -> (* capabilities *)
    Lwt_io.write oc (Printf.sprintf "astarttls starttls\r\n") >>= fun () ->
    starttls_client ip sock () >>= fun (ic,oc) ->
    f ic oc
  ) ()

let try_close ch =
  catch (fun() -> Lwt_io.close ch) (fun _ -> return())

let start_tls ip port f =
  X509_lwt.authenticator `No_authentication_I'M_STUPID >>= fun authenticator ->
  let config = Tls.Config.(client ~authenticator ~ciphers:Ciphers.supported()) in
  Tls_lwt.connect_ext config (ip,port) >>= fun (ic,oc) -> 
  Lwt_io.read_line ic >>= fun _ -> (* capabilities *)
  f ic oc >>= fun () ->
  try_close ic >>= fun () -> try_close oc

(* maildir has flat structure with mailboxes represented as .dirname and nested
 * mailboxes as .dirname.dirname1 ..
 * inbox is in the root, messages are store in cur and new folders 
 *)
let maildir_fold dir f =
  return ()

let imap_fold host port f =
  let open Socket_utils in
  let connected = ref false in
  Printf.fprintf stderr "Please enter user name:%!";
  Lwt_io.read_line Lwt_io.stdin >>= fun user ->
  Printf.fprintf stderr "Please enter password:%!";
  echo_on false >>= fun () ->
  Lwt_io.read_line Lwt_io.stdin >>= fun pswd -> 
  echo_on true >>= fun () ->
  let ports =
  match port with
  | None -> [993;143]
  | Some port -> [port]
  in
  (* get email servers *)
  catch (fun () -> Lwt_unix.gethostbyname host) 
    (fun ex -> Printf.fprintf stderr "failed to resolve host %s: %s\n%!" host
    (Printexc.to_string ex); raise ex) >>= fun entries ->
  let ips = Array.to_list entries.Unix.h_addr_list in
  Printf.fprintf stderr "domain %s resolution, ip entries found %d\n%!" host (List.length ips);
  let loop ports connect =
    Lwt_list.exists_s (fun ip ->
      let ip = Unix.string_of_inet_addr ip in
      Lwt_list.exists_s (fun port ->
        connect ip port
      ) ports
    ) ips
  in
  Lwt_list.exists_s (fun (ssl,ports) ->
    loop ports (fun ip port ->
      let start = 
        if ssl then (
          if port = 993 then
            start_tls ip port
          else
            start_ssl ip port
        ) else 
          start_plain ip port
      in
      catch (fun () ->
        start (fun ic oc ->
          Printf.fprintf stderr "connected to %s:%d\n%!" ip port;
          connected := true;
          capability ic oc >>= fun () ->
          login user pswd ic oc >>= fun () ->
          list host ic oc >>= fun l ->
          Printf.fprintf stderr "--- started processing ---\n%!";
          Lwt_list.iter_s (fun (cnt,mailbox) ->
            select ic oc mailbox >>= fun msgs ->
            if msgs = 0 then (
              Printf.fprintf stderr "%s, 0 messages\n%!" mailbox;
              return ()
            ) else (
              Printf.fprintf stderr "%s, %d messages\n%!" mailbox msgs;
              fetch_unique ic oc (Some mailbox) cnt f
            )
          ) l >>= fun() ->
          logout oc 
        ) >>= fun () ->
        return true
      ) (fun ex -> Printf.fprintf stderr "%s\n%!" (Printexc.to_string
      ex); if !connected then raise ex; return false)
    )
  ) [true,ports] >>= fun res ->
  if res = false then
    Printf.fprintf stderr "failed to get messages\n%!";
  return ()

let mbox_fold file f =
  Lwt_unix.stat file >>= fun st ->
  Printf.printf "archive size: %l\n%!" st.Unix.st_size;
  Utils.fold_email_with_file file (fun (cnt,mailbox) message ->
    f cnt mailbox message >>= fun () ->
    return (`Ok (cnt + 1,mailbox))
  ) (1,None) >>= fun _ ->
  return ()

let labels_from_mailbox mailbox =
  if Re.execp (Re_posix.compile_pat "[[]Gmail[]]") mailbox = false then (
    let l = Re.split (Re_posix.compile_pat "/") mailbox in
    String.concat "," (List.map get_mailbox l)
  ) else 
    get_mailbox mailbox

let () =
  let open Parsemail.Mailbox in
  commands (fun arch echo ->
  echoterm := echo;
  Lwt_main.run (
    mailboxes_init();
    let t = Unix.gettimeofday () in
    let (fold,download) = 
    match arch with
    | `Mbox file -> mbox_fold file,false
    | `Imap (host,port) -> imap_fold host port,false
    | `ImapDld (host,port) -> imap_fold host port,true
    | `Maildir _ -> raise InvalidCommand
    in
    fold (fun cnt mailbox message_s ->
      Printf.fprintf stderr "%d %d\r%!" cnt (String.length message_s);
      catch (fun () ->
        if download = false then (
          let message = Utils.make_email_message message_s in
          let email = message.Mailbox.Message.email in
          let headers = Email.header email in
          (*Printf.printf "headers:\n%s\n%!" (headers_to_string headers);*)
          let headers_l = Header.to_list headers in
          (* select interesting headers, update if more stats is needed *)
          let headers_m = headers_to_map headers_l in
          Printf.printf "--> start\n%!";
          Printf.printf "Full Message: %d %d\n" (String.length message_s) (len_compressed message_s);
          Printf.printf "Hdrs\n%!";
          Printf.printf "from: %s\n%!" (_from headers_m);
          Printf.printf "to: %s\n%!" (_to headers_m);
          Printf.printf "cc: %s\n%!" (_cc headers_m);
          Printf.printf "date: %s\n%!" (get_header_m headers_m "date");
          Printf.printf "subject: %s\n%!" (_subject headers_m);
          begin
          match mailbox with
          | Some mailbox -> Printf.printf "mailbox: %s\n%!" (labels_from_mailbox mailbox)
          | None -> Printf.printf "labels: %s\n%!" (_gmail_labels headers_m)
          end;
          Printf.printf "messageid: %s\n%!" (_messageid headers_m);
          Printf.printf "inreplyto: %s\n%!" (_inreplyto headers_m);
          Printf.printf "Parts:\n%!";
          walk email 0 0 false;
          Printf.printf "<-- end\n%!";
          return ()
        ) else (
          Printf.printf "%s\r\n%!" (Utils.make_postmark message_s);
          Printf.printf "X-Gmail-Labels: %s\r\n%!" (opt_val mailbox);
          Printf.printf "%s%!" message_s;
          if String.get message_s (String.length message_s - 1) <> '\n' then
            Printf.printf "\r\n";
          return ()
        )
      ) (fun ex -> if download = false then Printf.printf "<-- end failed to process: %s\n%!"
          (Printexc.to_string ex) else Printf.fprintf stderr "failed to parse the
          message\n%!"; return ())
    ) >>= fun () ->
    Printf.fprintf stderr "processing time: %d seconds\n%!" 
      (int_of_float (Unix.gettimeofday() -. t));
    return ()
  ) 
  )
