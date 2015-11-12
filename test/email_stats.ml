open Lwt
open X509.Encoding.Pem
open Imaplet
open Commands
open Parsemail

exception InvalidCommand
exception Failed
exception LoginFailed

let echoterm = ref false

let opt_val = function
  | None -> raise Failed
  | Some v -> v

let sprf = Printf.sprintf

let try_close ch =
  catch (fun() -> Lwt_io.close ch) (fun _ -> return())

(* in/out buffer if compression is enabled *)
let compression = ref false
let compr_strm = Zlib.deflate_init 6 false
let uncompr_strm = Zlib.inflate_init false
let inbuffer = ref (Buffer.create 100)
let buff_size = 1024
let resp_chan = ref None
let uncompr_waiter = ref None

(* continous compression stream for the session's duration *)
let _compress buffin =
  let len = String.length buffin in
  let buffout = String.create (2*len) in
  let (fi,bin,bout) = Zlib.deflate compr_strm buffin 0 len 
      buffout 0 (2*len) Zlib.Z_SYNC_FLUSH in
  String.sub buffout 0 bout

(* uncompress *)
let inflate oc_pipe inbuff =
  let outbuff = String.create buff_size in
  let rec _inflate inbuff offset len =
    let (fi, orig, size) = Zlib.inflate uncompr_strm inbuff offset len 
      outbuff 0 buff_size Zlib.Z_SYNC_FLUSH in
    Lwt_io.write oc_pipe (String.sub outbuff 0 size) >>
    Lwt_io.flush oc_pipe >>= fun () ->
    let offset = offset + orig in
    let len = len - orig in
    if fi then ( (* this should not happen? *)
      Printf.fprintf stderr "##### inflate stream is finished\n%!";
      Buffer.clear !inbuffer;
      if len <> 0 then (
        Buffer.add_string !inbuffer (String.sub inbuff offset len)
      );
      Zlib.inflate_end uncompr_strm;
      return ()
    ) else if len = 0 then ( 
      return ()
    ) else (
      _inflate inbuff offset len
    )
  in
  _inflate inbuff 0 (String.length inbuff)

(* reading/uncompression is done in chunks and
 * part of the chunk could be next compressed data stream
 * so the part is stored in the cache *)
let read_cache ic_net =
  if Buffer.length !inbuffer > 0 then (
    let contents = Buffer.contents !inbuffer in
    Buffer.clear !inbuffer;
    return (Some contents)
  ) else (
    let buff = String.create buff_size in
    Lwt_io.read_into ic_net buff 0 buff_size >>= function
    | 0 -> 
      Printf.fprintf stderr "##### read size 0\n%!";
      return None
    | size -> 
      let str = String.sub buff 0 size in
      return (Some str)
  )

let start_async_uncompr ic_net oc_pipe =
  (* recusevily uncompress chunks of data *)
  let rec read () =
    read_cache ic_net >>= function
    | None -> 
      Printf.fprintf stderr "##### reading from cache failed\n%!";
      try_close oc_pipe >>
      try_close (opt_val !resp_chan) 
    | Some buff -> 
      inflate oc_pipe buff >>
      read ()
  in
  read ()

(* read either from the network or from the uncompressed pipe 
 * if count is not requested then read the line otherwise 
 * read the specified count
 *)
let resp_read ?count () =
  let ic = opt_val !resp_chan in
  begin
  match count with 
  | None -> Lwt_io.read_line ic 
  | Some count -> 
    let buff = String.create count in
    Lwt_io.read_into_exactly ic buff 0 count >>
    return buff
  end >>= fun buff ->
  if !echoterm then
    Printf.fprintf stderr "%s\n%!" buff;
  return buff

let echo_on on =
  Lwt_unix.system (Printf.sprintf "stty %secho" (if on then "" else "-")) >>=
    fun _ -> return ()

let gmail host = if host = "imap.gmail.com" || host = "192.168.2.2" then true else false

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

let rec args i archive echo nocompress outfile unique =
  if i >= Array.length Sys.argv then
    archive,echo,nocompress,outfile,unique
  else
    match Sys.argv.(i) with 
    | "-archive" -> args (i+2) (Some (get_arch Sys.argv.(i+1))) echo nocompress outfile unique
    | "-echo" -> args (i+1) archive true nocompress outfile unique
    | "-no-compress" -> args (i+1) archive echo true outfile unique
    | "-outfile" -> args (i+2) archive echo nocompress (Some Sys.argv.(i+1)) unique
    | "-unique" -> args (i+1) archive echo nocompress outfile true
    | _ -> raise InvalidCommand

let commands f =
  try 
    let archive,echo,nocompress,outfile,unique = args 1 None false false None false in
    let resume =
    match outfile with
    | Some file ->
      if String.sub file 0 7 = "resume:" then (
        let file = String.sub file 7 (String.length file - 7) in
        Some file
      ) else (
        None
      )
    | None -> None
    in
    f (opt_val archive) echo nocompress outfile resume unique
  with 
    | Failed|InvalidCommand ->
      Printf.printf 
        "usage: email_stats -archive [mbox:file|imap[-dld]:server:port] [-outfile
        [[resume:]file]] [-echo] [-no-compress] [-unique]\n%!"
    | _ -> ()

let email_addr = ref (Hashtbl.create 0)
let attachments = ref (Hashtbl.create 0)
let mailboxes = ref (Hashtbl.create 0)
let subject = ref (Hashtbl.create 0)
let messageid = ref (Hashtbl.create 0)

let init_hashtbl_all size =
  email_addr := Hashtbl.create size;
  attachments := Hashtbl.create size;
  mailboxes := Hashtbl.create size;
  subject := Hashtbl.create size;
  messageid := Hashtbl.create size

let get_unique key tbl =
  if key = "" then
    ""
  else (
    if Hashtbl.mem !tbl key then
      Hashtbl.find !tbl key
    else (
      let cnt = string_of_int ((Hashtbl.length !tbl) + 1) in
      Hashtbl.add !tbl key cnt;
      cnt
    )
  )

let get_addr key =
  get_unique key email_addr

let get_attach key =
  get_unique key attachments 

let get_mailbox key =
  let key = Re.replace_string (Re_posix.compile_pat "^\"|\"$") ~by:"" key in
  get_unique key mailboxes 

let get_subject key =
  get_unique key subject

let get_messageid key =
  get_unique key messageid

let headers_to_map headers =
  List.fold_left (fun tbl (n,v) ->
    let n = String.lowercase n in
    if n = "from" || n = "to" || n = "cc" || n = "subject" || n = "object-id" || 
        n = "in-reply-to" || n = "x-gmail-labels" || n = "content-type" || 
        n = "date" || n = "message-id" then (
      Hashtbl.add tbl n (String.trim v);
      tbl
    ) else
      tbl
  ) (Hashtbl.create 10) headers

let get_header_m headers name =
  try 
    Hashtbl.find headers name
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
  begin
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
  end

let _messageid headers =
  let h = get_header_m headers "message-id" in
  (get_messageid h)

let messageid_unique headers =
  let h = get_header_m headers "message-id" in
  let res = ((Hashtbl.mem !messageid h) = false) in
  res

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

let rec walk outc email id part multipart =
  let write = Lwt_io.write outc in
  let headers = Header.to_list (Email.header email) in
  let headers_s = headers_to_string (Email.header email) in
  let attach,rfc822 = get_hdr_attrs headers in
  write (sprf "part: %d\n" part) >>= fun () ->
  write (sprf "headers: %d %d %d\n" (List.length headers) (String.length headers_s) 
    (len_compressed headers_s)) >>= fun () ->
  begin
  match Email.content email with
  | `Data _ ->
    let content = email_raw_content email in
    if multipart && rfc822 then (
      write (sprf "start rfc822: %d\n" id) >>= fun () ->
      let email = Email.of_string content in
      walk outc email (id+1) 0 multipart >>= fun () ->
      write (sprf "end rfc822: %d\n" id)
    ) else if attach then (
      let sha = Imap_crypto.get_hash ~hash:`Sha1 content in
      write (sprf "attachment: %s %d %d\n" (get_attach sha) 
        (String.length content) (len_compressed content))
    ) else (
      write (sprf "body: %d %d\n" (String.length content) (len_compressed content))
    )
  | `Message _ -> assert (false)
  | `Multipart elist ->
    write (sprf "start multipart %d %d:\n%!" id (List.length elist)) >>
    Lwt_list.fold_left_s (fun part email ->
      walk outc email (id+1) part true >>= fun () ->
      return (part + 1)
    ) 0 elist >>= fun _ ->
    write (sprf "end multipart %d\n%!" id)
  end >> Lwt_io.flush outc

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
  let msg = if !compression then _compress msg else msg in
  Lwt_io.write oc msg >>
  Lwt_io.flush oc

let while_not_ok () =
  let rec _read () =
    resp_read () >>= fun res ->
    if ok res then
      return ()
    else if bad res || no res then
      raise Failed
    else 
      _read ()
  in
  _read ()

let tag = ref 0

let get_tag () =
  tag := !tag + 1;
  Printf.sprintf "a%04d" !tag

(*
 a capability
 * CAPABILITY IMAP4rev1 UNSELECT IDLE NAMESPACE QUOTA ID XLIST CHILDREN
   X-GM-EXT-1 UIDPLUS COMPRESS=DEFLATE ENABLE MOVE CONDSTORE ESEARCH UTF8=ACCEPT
   LIST-EXTENDED LIST-STATUS
 a OK Success
 *)
let capability oc =
  write oc (Printf.sprintf "%s capability\r\n" (get_tag())) >>= fun () ->
  resp_read () >>= fun res ->
  let l = Re.split (Re_posix.compile_pat "[ ]+") res in
  resp_read () >>= fun res ->
  if ok res then
    return l
  else
    raise Failed

let compress_deflate ic_net oc_net =
  write oc_net (Printf.sprintf "%s compress deflate\r\n" (get_tag())) >>= fun () ->
  while_not_ok () >>= fun () ->
  compression := true;
  let (ic_pipe,oc_pipe) = Lwt_io.pipe () in
  let (waiter,wakener) = Lwt.task () in
  let uncompr = (start_async_uncompr ic_net oc_pipe >>= fun () ->
    wakeup wakener (); waiter) in
  uncompr_waiter := Some uncompr;
  async(fun () -> catch(fun() -> uncompr)(fun _ -> return()));
  resp_chan := Some ic_pipe;
  return ()

let cancel_uncompr () =
  match !uncompr_waiter with
  | None -> ()
  | Some waiter -> 
    Lwt.cancel waiter;
    uncompr_waiter := None

let login user pswd nocompress ic oc =
  catch (fun() ->
    write oc (Printf.sprintf "%s login %s %s\r\n" (get_tag()) user pswd) >>= fun () ->
    while_not_ok () >>= fun () ->
    begin
    if nocompress = false then (
      capability oc >>= fun caps ->
      if List.exists (fun c -> String.lowercase c = "compress=deflate") caps then (
        compress_deflate ic oc >>= fun () ->
        Printf.fprintf stderr "compression enabled\n%!";
        return ()
      ) else (
        Printf.fprintf stderr "compression is not supported or failed to enable\n%!";
        return ()
      )
    ) else (
      return ()
    )
    end
  ) (fun _ -> raise LoginFailed)

let logout oc =
  write oc (Printf.sprintf "%s logout\r\n" (get_tag()))

(* STATUS "ATT" (MESSAGES 10) *)
let status oc mailbox =
  write oc (Printf.sprintf "%s status %s (messages)\r\n" (get_tag()) mailbox) >>= fun () ->
  resp_read () >>= fun res ->
  let msgs = 
    try
      let re = Re_posix.compile_pat ~opts:[`ICase] "messages ([0-9]+)\\)$" in
      let subs = Re.exec re res in
      Re.get subs 1
    with Not_found -> "-" 
  in
  while_not_ok () >>= fun () ->
  return msgs

let checkresume unique = function
  | None -> return (Hashtbl.create 0,Int64.zero)
  | Some file ->
    if unique then
      messageid := Hashtbl.create 50_000;
    Printf.fprintf stderr "checking resume point\n%!";
    let labels = "X-Gmail-Labels: " in
    let lenlbl = String.length labels in
    let msgid = "message-id: " in
    let lenid = String.length msgid in
    Lwt_io.with_file ~flags:[Unix.O_RDONLY;Unix.O_NONBLOCK] ~mode:Lwt_io.Input file (fun ic ->
      Lwt_io.length ic >>= fun length ->
      let length = Int64.to_float length in
      let rec _read mailboxes offset =
        Lwt_io.read_line_opt ic >>= function
        | None -> return (mailboxes,offset)
        | Some line ->
        let len = String.length line in
        if unique && len > lenid && (String.lowercase (String.sub line 0 lenid)) = msgid then (
          let id = String.trim (String.sub line lenid (len - lenid)) in
          let _ = get_messageid id in ()
        );
        let re_postmark = Re_posix.compile_pat ~opts:[`ICase] 
          "^(From [^ \r\n]+ (mon|tue|wed|thu|fri|sat|sun) (jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec))" in
        let offset =  (* offset of last from, last message will be overwritten since
        it could be partial *)
        if Re.execp re_postmark line then (
          Int64.sub (Lwt_io.position ic) (Int64.of_int (String.length line))
        ) else 
          offset
        in
        if len > lenlbl && String.sub line 0 lenlbl = labels then (
          let label = String.trim (String.sub line lenlbl (len - lenlbl)) in
          let position = Int64.to_float (Lwt_io.position ic) in
          let pct = (100. *. position) /. length in
          if Hashtbl.mem mailboxes label = false then (
            Printf.fprintf stderr "%%%.02f, %d, %s                   \r%!" pct 1 label;
            Hashtbl.add mailboxes label 1;
            _read mailboxes offset
          ) else (
            let cnt = Hashtbl.find mailboxes label in
            Printf.fprintf stderr "%%%.02f, %d, %s                   \r%!" pct (cnt+1) label;
            Hashtbl.replace mailboxes label (cnt + 1);
            _read mailboxes offset
          )
        ) else 
          _read mailboxes offset
      in
      _read (Hashtbl.create 0) Int64.zero
    )

(*
   * LIST (\HasNoChildren) "/" "Bulk Mail"
   * LIST (\HasChildren) "/" "Cambridge"
   * LIST (\HasNoChildren) "/" "Cambridge/Entertainment"
 *)
let list host resume oc = 
  write oc (Printf.sprintf "%s list \"\" *\r\n" (get_tag())) >>= fun () ->
    let re = Re_posix.compile_pat ~opts:[`ICase] "^\\* list \\(([^)]+)\\) [^ ]+ ((\"[^\"]+\")|([^\t ]+))$" in
  Printf.fprintf stderr "mailboxes to process:\n%!";
  let rec _read l =
    resp_read () >>= fun res ->
    if ok res then
      return l
    else (
      let subs = Re.exec re res in
      let mailbox = Re.get subs 2 in
      let attrs = Re.split (Re_posix.compile_pat " ") (Re.get subs 1) in
      if List.exists (fun attr -> (String.lowercase attr) = "\\noselect") attrs ||
          mailbox = "\"[Gmail]/All Mail\"" || mailbox = "\"[Gmail]/Important\"" || 
          mailbox = "\"[Gmail]/Starred\"" then
        _read l
      else (
        _read (mailbox :: l)
      )
    )
  in 
  _read [] >>= fun l ->
  let l = List.sort String.compare l in
  let l = 
    if gmail host then List.concat [l;["\"[Gmail]/Important\"";"\"[Gmail]/All Mail\""]] else l
  in
  let total = ref 0 in
  Lwt_list.map_s (fun m ->
    status oc m >>= fun msgs ->
    Printf.fprintf stderr "%s, %s messages\n%!" m msgs;
    let msgs = int_of_string msgs in
    total := !total + msgs;
    let start =
      if Hashtbl.mem resume m then
        ref (Hashtbl.find resume m)
      else
        ref 1
    in
    return (start,msgs,m)
  ) l >>= fun l ->
  Printf.fprintf stderr "Total messages: %d\n%!" !total;
  return l

let select oc mailbox =
  write oc (Printf.sprintf "%s select %s\r\n" (get_tag()) mailbox) >>= fun () ->
  let rec _read msgs =
    resp_read () >>= fun res ->
    if ok res then
      return msgs
    else (
      let ex,m = exists res in
      let msgs = if ex then m else msgs in
      _read msgs
    )
  in 
  _read 0

let re_fetch = Re_posix.compile_pat ~opts:[`ICase] 
  "^\\* ([0-9]+) fetch [^}]+[{]([0-9]+)[}]$" (* * 5 FETCH (BODY[] {1208} *)

let write_fetch oc seq part =
  let seq =
  match seq with
  | `All -> "1:*"
  | `Start i -> sprf "%d:*" i
  | `Range (i1,i2) -> sprf "%d:%d" i1 i2
  | `Single i -> sprf "%d" i
  in
  let part =
  match part with 
  | `Header -> "header"
  | `Text -> "text"
  | `Body -> ""
  | `HeaderMessageID -> "header.fields (Message-ID)"
  in
  write oc (Printf.sprintf "%s fetch %s body.peek[%s]\r\n" (get_tag()) seq part)

let get_literal_data res =
  let subs = Re.exec re_fetch res in
  let id = int_of_string (Re.get subs 1) in
  let count = int_of_string (Re.get subs 2) in
  resp_read ~count () >>= fun data ->
  return (id,data)

let get_part () =
  resp_read () >>= fun res ->
  get_literal_data res >>= fun (_,msg) ->
  resp_read () >>= fun res -> (* \r\n after the literal *)
  if res <> ")" then raise Failed;
  resp_read () >>= fun res -> (* closing parenth *)
  if ok res = false then raise Failed;
  return msg

let fetch_part oc i part =
  write_fetch oc (`Single i) part >>
  get_part ()

let messageid_from_headers headers =
  let re = Re_posix.compile_pat ~opts:[`ICase] "message-id: ([^ \r\n]+)" in
  try
    let subs = (Re.exec re headers) in
    Re.get subs 1
  with Not_found -> ""

let fetch_unique oc start mailbox cnt f =
  let rec _fetch i =
    if i > cnt then 
      return ()
    else (
      fetch_part oc i `HeaderMessageID >>= fun headers ->
      let msgid = messageid_from_headers headers in
      if msgid <> "" && Hashtbl.mem !messageid msgid then (
        _fetch (i + 1)
      ) else (
        let _ = get_messageid msgid in
        fetch_part oc i `Body >>= fun message ->
        f i mailbox message >>= fun () ->
        start := i;
        _fetch (i + 1)
      )
    )
  in
  _fetch !start

let fetch_unique_all oc start mailbox cnt f =
  if !start >= cnt then
    return ()
  else (
    write_fetch oc (`Start !start) `HeaderMessageID >>= fun () ->
    let rec _read seq =
      resp_read () >>= fun res ->
      if ok_ex res then (
        return seq
      ) else if Re.execp re_fetch res then (
        get_literal_data res >>= fun (id,headers) ->
        let msgid = messageid_from_headers headers in
        if msgid <> "" && Hashtbl.mem !messageid msgid then (
          _read seq
        ) else (
          let _ = get_messageid msgid in
          _read (id::seq)
        )
      ) else  (
        _read seq
      )
    in
    _read [] >>= fun seq ->
    Lwt_list.fold_right_s (fun s _ ->
      fetch_part oc s `Body >>= fun message ->
      f s mailbox message >>= fun () ->
      start := s;
      return ()
    ) seq ()
  )

let fetch oc start mailbox cnt f =
  if start >= cnt then
    return ()
  else (
  write_fetch oc (`Start start) `Body >>= fun () ->
  let rec _read () =
    resp_read () >>= fun res ->
    if ok_ex res then (
      return () 
    ) else if Re.execp re_fetch res then (
      get_literal_data res >>= fun (id,msg) ->
      f id mailbox msg >>= fun () ->
      _read ()
    ) else  (
      _read ()
    )
  in
  _read ()
  )

let start_plain ip port f =
  Printf.fprintf stderr "connecting open\n%!";
  let open Socket_utils in
  client_send (`Inet (ip,port)) (fun res sock ic oc -> 
    catch (fun () ->
      Lwt_io.read_line ic >>= fun _ -> (* capabilities *)
      f ic oc >>= fun () ->
      return None
    ) (fun ex -> return (Some ex))
  ) None >>= function
  | Some ex -> Printf.fprintf stderr "startplain: %s\n%!" (Printexc.to_string ex); raise ex
  | None -> return ()

let start_ssl ip port f =
  Printf.fprintf stderr "connecting ssl\n%!";
  let open Socket_utils in
  client_send (`Inet (ip,port)) (fun res sock ic oc -> 
    catch (fun () ->
      Lwt_io.read_line ic >>= fun _ -> (* capabilities *)
      Lwt_io.write oc (Printf.sprintf "astarttls starttls\r\n") >>= fun () ->
      starttls_client ip sock () >>= fun (ic,oc) ->
      f ic oc >>= fun () ->
      return None
    ) (fun ex -> return (Some ex))
  ) None >>= function
  | Some ex -> Printf.fprintf stderr "startssl: %s\n%!" (Printexc.to_string ex); raise ex
  | None -> return ()

let start_tls ip port f =
  Printf.fprintf stderr "connecting tls\n%!";
  X509_lwt.authenticator `No_authentication_I'M_STUPID >>= fun authenticator ->
  let config = Tls.Config.(client ~authenticator ~ciphers:Ciphers.supported()) in
  Tls_lwt.connect_ext config (ip,port) >>= fun (ic,oc) -> 
  catch (fun() ->
    Lwt_io.read_line ic >>= fun _ -> (* capabilities *)
    f ic oc
  ) 
  (fun ex -> 
    Printf.fprintf stderr "starttls: %s\n%!" (Printexc.to_string ex);
    try_close ic >>= fun () -> try_close oc >>= fun () -> raise ex
  )

(* maildir has flat structure with mailboxes represented as .dirname and nested
 * mailboxes as .dirname.dirname1 ..
 * inbox is in the root, messages are store in cur and new folders 
 *)
let maildir_fold dir f =
  return ()

let imap_fold host port nocompress resume unique download f =
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
  let rec run ip port ssl mailboxes attempts =
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
        compression := false;
        resp_chan := Some ic;
        login user pswd nocompress ic oc >>= fun () ->
        begin
        if mailboxes = [] then
          list host resume oc
        else
          return mailboxes
        end >>= fun mailboxes ->
        if unique && download && Hashtbl.length !messageid = 0 then (
          let total = List.fold_left (fun acc (_,cnt,_) -> acc+cnt) 0 mailboxes in
          messageid := Hashtbl.create total
        );
        Printf.fprintf stderr "--- started processing ---\n%!";
        Lwt_list.iter_s (fun (start,cnt,mailbox) ->
          if cnt = 0 then (
            Printf.fprintf stderr "%s, 0 messages\n%!" mailbox;
            return ()
          ) else if !start >= cnt then (
            return () (* was already processed, retrying *)
          ) else (
            select oc mailbox >>= fun msgs ->
            Printf.fprintf stderr "%s, %d messages\n%!" mailbox msgs;
            if mailbox = "\"[Gmail]/All Mail\"" && unique then
              fetch_unique_all oc start (Some mailbox) cnt f
            else
              fetch oc !start (Some mailbox) cnt f
          )
        ) mailboxes >>= fun() ->
        logout oc 
      ) >>= fun () ->
      return true
    ) 
    (fun ex -> 
      cancel_uncompr ();
      if ex <> LoginFailed && !connected && attempts < 10 then (
        Printf.fprintf stderr "retrying %d\n%!" attempts; 
        run ip port ssl mailboxes (attempts + 1) 
      ) else 
        return false
    )
  in
  Lwt_list.exists_s (fun (ssl,ports) ->
    loop ports (fun ip port ->
      run ip port ssl [] 1
    )
  ) [true,ports] >>= fun res ->
  cancel_uncompr ();
  if res = false then
    Printf.fprintf stderr "failed to get messages\n%!";
  return ()

let mbox_fold file f =
  Utils.fold_email_with_file file (fun (cnt,mailbox) message ->
    f cnt mailbox message >>= fun () ->
    return (`Ok (cnt + 1,mailbox))
  ) (1,None) >>= fun (cnt,_) ->
  Printf.fprintf stderr "total messages processed: %d\n%!" cnt;
  return ()

let labels_from_mailbox mailbox =
  if Re.execp (Re_posix.compile_pat "[[]Gmail[]]") mailbox = false then (
    let l = Re.split (Re_posix.compile_pat "/") mailbox in
    String.concat "," (List.map get_mailbox l)
  ) else 
    get_mailbox mailbox

let () =
  let open Parsemail.Mailbox in
  commands (fun arch echo nocompress outfile resume unique ->
  echoterm := echo;
  Lwt_main.run (
    let flags = [Unix.O_NONBLOCK;Unix.O_CREAT;Unix.O_WRONLY] in
    let flags = if resume = None then Unix.O_TRUNC :: flags else flags in
    let with_file = 
      if outfile = None then 
        (fun f -> f Lwt_io.stdout)
      else
        Lwt_io.with_file ~flags ~mode:Lwt_io.Output (opt_val outfile)
    in
    with_file (fun outc ->
    let sprf = Printf.sprintf in
    let write = Lwt_io.write outc in
    let progress = ref 0 in
    let t = Unix.gettimeofday () in
    begin
    match arch with
    | `Mbox file ->
        Lwt_unix.stat file >>= fun st ->
        write (sprf "archive size: %l\n%!" st.Unix.st_size) >>= fun () ->
        return (mbox_fold file,false,st.Unix.st_size)
    | `Imap (host,port) -> 
      let resume = Hashtbl.create 0 in
      return (imap_fold host port nocompress resume unique false,false,0)
    | `ImapDld (host,port) -> 
      checkresume unique resume >>= fun (resume,offset) ->
      (if Int64.compare offset Int64.zero > 0 then Lwt_io.set_position outc offset else return ()) >>= fun () ->
      return (imap_fold host port nocompress resume unique true,true,0)
    | `Maildir _ -> raise InvalidCommand
    end >>= fun (fold,download,size) ->
    if download = false then init_hashtbl_all 50_000;
    mailboxes_init();
    fold (fun cnt mailbox message_s ->
      catch (fun () ->
        if download = false then (
          progress := !progress + (String.length message_s);
          Printf.fprintf stderr "%%%02.0f %d %d     \r%!" 
            ((100. *. (float_of_int !progress))/.(float_of_int size)) cnt (String.length message_s);
          let message = Utils.make_email_message message_s in
          let email = message.Mailbox.Message.email in
          let headers = Email.header email in
          let headers_l = Header.to_list headers in
          (* select required headers, update if more stats is needed *)
          let headers_m = headers_to_map headers_l in
          if messageid_unique headers_m then (
            write "--> start\n" >>
            write (sprf "Full Message: %d %d\n" (String.length message_s) (len_compressed message_s)) >>
            write "Hdrs\n" >>
            write (sprf "from: %s\n" (_from headers_m)) >>
            write (sprf "to: %s\n" (_to headers_m)) >>
            write (sprf "cc: %s\n" (_cc headers_m)) >>
            write (sprf "date: %s\n" (get_header_m headers_m "date")) >>
            write (sprf "subject: %s\n" (_subject headers_m)) >>
            begin
            match mailbox with
            | Some mailbox -> write (sprf "mailbox: %s\n" (labels_from_mailbox mailbox))
            | None -> write (sprf "labels: %s\n" (_gmail_labels headers_m))
            end >>
            write (sprf "messageid: %s\n" (_messageid headers_m)) >>
            write (sprf "inreplyto: %s\n" (_inreplyto headers_m)) >>
            write "Parts:\n" >>
            walk outc email 0 0 false >>
            write "<-- end\n" >>
            Lwt_io.flush outc
          ) else (
            return ()
          )
        ) else (
          Printf.fprintf stderr "%d %d     \r%!" cnt (String.length message_s);
          write (sprf "%s\r\n" (Utils.make_postmark message_s)) >>
          write (sprf "X-Gmail-Labels: %s\r\n" (opt_val mailbox)) >>
          write message_s >>
          begin
          if String.get message_s (String.length message_s - 1) <> '\n' then
            write "\r\n"
          else
            return ()
          end >>
          Lwt_io.flush outc
        )
      ) (fun ex -> if download = false then write (sprf "<-- end failed to process: %s\n%!"
          (Printexc.to_string ex)) else (Printf.fprintf stderr "failed to parse the
          message\n%!"; return ()))
    ) >>= fun () ->
    Printf.fprintf stderr "processing time: %d seconds\n%!" 
      (int_of_float (Unix.gettimeofday() -. t));
    return ()
    )
  ) 
  )
