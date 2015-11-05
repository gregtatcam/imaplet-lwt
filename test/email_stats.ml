open Lwt
open X509.Encoding.Pem
open Imaplet
open Commands
open Parsemail

module MapStr = Map.Make(String)

exception InvalidCommand
exception FailedPubKey
exception FailedLogin
exception Failed

let echoterm = ref false

let cert = 
"-----BEGIN CERTIFICATE-----
MIICWDCCAcGgAwIBAgIJAPHg+v48OaXDMA0GCSqGSIb3DQEBBQUAMEUxCzAJBgNV
BAYTAkFVMRMwEQYDVQQIDApTb21lLVN0YXRlMSEwHwYDVQQKDBhJbnRlcm5ldCBX
aWRnaXRzIFB0eSBMdGQwHhcNMTUwODI1MDkzMjI5WhcNMTUwOTI0MDkzMjI5WjBF
MQswCQYDVQQGEwJBVTETMBEGA1UECAwKU29tZS1TdGF0ZTEhMB8GA1UECgwYSW50
ZXJuZXQgV2lkZ2l0cyBQdHkgTHRkMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKB
gQDpZlJPshUhIPVMxOHq+d1R1L4CiiyjEyapugvyz2QHAhjtu6JN37BhdjTsXWLT
jgd9FsBD5cfat6+8RZw1+UOiQQvT4wB3fXCTpy8zIfwVvJ4gR/mCwLIVyCSZHikZ
KdYB1tjLayD67sO4XGhSZX1cnGQ8xBiTIphkFdIa/hyLIwIDAQABo1AwTjAdBgNV
HQ4EFgQUtLWV6EsgxskDqIVBuodaUaqokcIwHwYDVR0jBBgwFoAUtLWV6EsgxskD
qIVBuodaUaqokcIwDAYDVR0TBAUwAwEB/zANBgkqhkiG9w0BAQUFAAOBgQCOZYxI
keLyha3YsuxHOpn3O2U6E4En0nOJg8zhvIxPu/erlTDiH/iBE5d8AsnV5MrSPKMj
bstEZzeFVO2ZXtr2V6lsTSo8pePLXwbKEqPjivvcPlLptlO+EVG2u1jCsVEO5Q1M
eZjV46t4w1tSsbwfnWz3xSnHViIWNJOao7jqzQ==
-----END CERTIFICATE-----"

let pub_of_cert_string cert =
  let cert = Certificate.of_pem_cstruct1 (Cstruct.of_string cert) in
  match (X509.public_key cert) with
  | `RSA pub_key -> pub_key
  | _ -> raise FailedPubKey

let echo_on on =
  (*
  Lwt_unix.tcgetattr Lwt_unix.stdin >>= fun io ->
  let io = {io with Unix.c_echo = on} in
  Lwt_unix.tcsetattr Lwt_unix.stdin Unix.TCSANOW io
  *)
  Lwt_unix.system (Printf.sprintf "stty %secho" (if on then "" else "-")) >>=
    fun _ -> return ()

let get_arch str =
  let re = Re_posix.compile_pat "^(mbox|maildir|imap):(.+)$" in
  let subs = Re.exec re str in
  let arch = Re.get subs 1 in
  let args = Re.get subs 2 in
  match arch with
  | "mbox" -> `Mbox args
  | "maildir" -> `Maildir args
  | "imap" ->
    let re = Re_posix.compile_pat "^([^:]+)(:([0-9]+))?$" in
    let subs = Re.exec re args in
    let host = Re.get subs 1 in
    let port = try Some (int_of_string (Re.get subs 3)) with Not_found -> None in
    `Imap (host,port)
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
  with _ ->
    Printf.printf "usage: email_stats -archive [mbox:file|imap:server:port:user:password]\n%!"


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
  get_unique key mailboxes_cnt mailboxes 

let get_subject key =
  get_unique key subject_cnt subject

let get_messageid key =
  get_unique key messageid_cnt messageid

let headers_to_map headers =
  List.fold_left (fun map (n,v) ->
    let n = String.lowercase n in
    if n = "from" || n = "to" || n = "cc" || n = "subject" || n = "object-id" || 
        n = "in-reply-to" || n = "x-gmail-labels" || n = "content-type" || n = "date" then
      MapStr.add n v map
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
    let h = String.lowercase h in
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
  let _ = get_mailbox "trash" in
  let _ = get_mailbox "junk" in
  let _ = get_mailbox "deleted" in
  let _ = get_mailbox "deleted messages" in
  let _ = get_mailbox "drafts" in ()

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
  write oc (Printf.sprintf "%s login %s %s\r\n" (get_tag()) user pswd) >>= fun () ->
  while_not_ok ic

(*
   * LIST (\HasNoChildren) "/" "Bulk Mail"
   * LIST (\HasChildren) "/" "Cambridge"
   * LIST (\HasNoChildren) "/" "Cambridge/Entertainment"
 *)
let list ic oc = 
  write oc (Printf.sprintf "%s list \"\" *\r\n" (get_tag())) >>= fun () ->
    let re = Re_posix.compile_pat "((\"[^\"]+\")|([^\t ]+))$" in
  let rec read l =
    read_line ~force:true ic >>= fun res ->
    if ok res then
      return l
    else (
      let subs = Re.exec re res in
      read ((Re.get subs 1) :: l)
    )
  in 
  read []

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

let fetch ic oc mailbox f =
  let re_fetch = Re_posix.compile_pat ~opts:[`ICase] 
    "^\\* ([0-9]+) fetch [^}]+[{]([0-9]+)[}]$" in (* * 5 FETCH (BODY[] {1208} *)
  write oc (Printf.sprintf "%s fetch 1:* body[]\r\n" (get_tag())) >>= fun () ->
  let rec read () =
    read_line ic >>= fun res ->
    if ok_ex res then (
      Printf.fprintf stderr "---------- returning from fetch\n%!";
      return () 
    ) else if Re.execp re_fetch res then (
      Printf.fprintf stderr "---------- fetch header\n%!";
      let subs = Re.exec re_fetch res in
      let id = int_of_string (Re.get subs 1) in
      let count = int_of_string (Re.get subs 2) in
      let msg = String.create count in
      Lwt_io.read_into_exactly ic msg 0 count >>= fun () ->
      read_line ic >>= fun _ -> (* new line *)
      read_line ic >>= fun _ -> (* closing bracket *)
      f id mailbox msg >>= fun () ->
      read ()
    ) else  (
      Printf.fprintf stderr "---------- something else\n%!";
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

let authenticator () =
  let cert = Certificate.of_pem_cstruct (Cstruct.of_string cert) in
  X509.Authenticator.chain_of_trust ~time:(Unix.gettimeofday()) cert

let start_tls ip port f =
  X509_lwt.authenticator `No_authentication_I'M_STUPID >>= fun authenticator ->
  let config = Tls.Config.(client ~authenticator ~ciphers:Ciphers.supported()) in
  Tls_lwt.connect_ext config (ip,port) >>= fun (ic,oc) -> 
  Lwt_io.read_line ic >>= fun _ -> (* capabilities *)
  f ic oc >>= fun () ->
  Lwt_io.close ic >>= fun () -> Lwt_io.close oc

let imap_fold host port f =
  let open Socket_utils in
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
  Lwt_unix.gethostbyname host >>= fun entries ->
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
          capability ic oc >>= fun () ->
          login user pswd ic oc >>= fun () ->
          list ic oc >>= fun l ->
          Lwt_list.iter_s (fun mailbox ->
            select ic oc mailbox >>= fun msgs ->
            if msgs = 0 then (
              Printf.fprintf stderr "%s, 0 messages\n%!" mailbox;
              return ()
            ) else (
              Printf.fprintf stderr "%s, %d messages\n%!" mailbox msgs;
              fetch ic oc mailbox f
            )
          ) l
        ) >>= fun () ->
        return true
      ) (fun ex -> Printf.fprintf stderr "exception: %s\n%!" (Printexc.to_string
      ex); return false)
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
    return (`Ok (cnt + 1,""))
  ) (1,"") >>= fun _ ->
  return ()

let () =
  let open Parsemail.Mailbox in
  commands (fun arch echo ->
  echoterm := echo;
  Lwt_main.run (
    mailboxes_init();
    let t = Unix.gettimeofday () in
    let fold = 
    match arch with
    | `Mbox file -> mbox_fold file
    | `Imap (host,port) -> imap_fold host port
    | `Maildir _ -> raise InvalidCommand
    in
    fold (fun cnt mailbox message_s ->
      Printf.fprintf stderr "%d\r%!" cnt;
      (*Printf.printf "message:\n%s\n%!" message_s;*)
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
      Printf.printf "labels: %s\n%!" (_gmail_labels headers_m);
      Printf.printf "messageid: %s\n%!" (_messageid headers_m);
      Printf.printf "inreplyto: %s\n%!" (_inreplyto headers_m);
      Printf.printf "Parts:\n%!";
      walk email 0 0 false;
      Printf.printf "<-- end\n%!";
      return ()
    ) >>= fun () ->
    Printf.fprintf stderr "processing time: %d seconds\n%!" 
      (int_of_float (Unix.gettimeofday() -. t));
    return ()
  ) 
  )
