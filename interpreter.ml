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
open Email_message
open Imaplet_types
open Regex
open Storage_meta
open Utils
open Dates
open Printf

exception InvalidSequence

exception ExecDone

exception InvalidMessage

module MapStr = Map.Make(String)

let map_find map key =
  if MapStr.mem key map then
    Some (MapStr.find key map)
  else
    None

type bodystr_fields = {
  btype: string;
  bsubtype: string;
  bparams: string;
  bid: string;
  bdescr: string;
  benc: string;
  bsize: string;
  blines: string;
  bdisp: string}

let rec raw_content email =
  let hdr_size = String.length (String_monoid.to_string (Header.to_string_monoid (Email.header email))) in
  let email_str = Email.to_string email in
  Str.last_chars email_str ((String.length email_str) - hdr_size)

let flags_to_string flags =
  List.fold_left (fun acc fl -> if acc = "" then fl_to_str fl else
    acc ^ " " ^ (fl_to_str fl)) "" flags

let media_type (email:Email.t) : Media_type.t =
  try
    let tl = Header.Content_type.all (Email.header email) in
    (List.nth tl 0)
  with _ -> (** set to text?? **)
    Media_type.of_string "text/plain; charset=us-ascii"

(** this might have to be trimmed, too much info, not in dovecot TBD**)
let mime_type (email:Email.t) : string =
  try
    Media_type.to_string (media_type email)
  with _ -> ""

let is_message_rfc822 (email:Email.t) : bool =
  (**Media_type.is_message_rfc2822 (media_type email) doesn't work?? TBD **)
  let media = media_type email in
  let t = (Media_type.mime_type media) in
  let st = (Media_type.mime_subtype media) in
  t = "message" && st = "rfc822"

(** get encapsulated message or None **)
let get_message_email (email:Email.t) (stream:Octet_stream.t) : Email.t option = 
  printf "get_message_email %b %s\n%!" (is_message_rfc822 email) (mime_type email);
  try
    if is_message_rfc822 email then (
      printf "get_message_email %s\n%!" (Octet_stream.to_string stream);
      Some (Email.of_octet_stream stream)
    ) else
      None
  with _ -> None

let headers_to_map (headers:Header.t) =
  let hl = Header.to_list headers in
  List.fold_left 
  (fun map (k,v) -> MapStr.add (String.lowercase k) (replace ~regx:" " ~tmpl:"" v) map) 
    MapStr.empty hl

let map_of_alist (l:string list) =
  List.fold_left (fun m i -> MapStr.add i () m) MapStr.empty l

let domatch (tomatch:string list) (header:string) : bool =
  list_find tomatch
    (fun regex -> match_regex ~case:false header ~regx:regex) 

let rec fold_email (email:Email.t) ~(init:'a) ~(f:('a -> Email.t -> 'a)) : 'a =
  let cont = Email.content email in
  match cont with
  | `Data cont -> (f init email)
  | `Message email -> fold_email email ~init ~f
  | `Multipart lemail -> 
    List.fold_left (fun acc email ->
      fold_email email ~init:acc ~f
    ) init lemail

let fold_email_headers ?(incl=MapStr.empty) ?(excl=MapStr.empty)
?(regex=false)
(headers:Header.t) ~(init:'a) ~(f:('a -> (string*string) -> 'a)) : 'a =
  let headers = (Header.to_list headers) in
  List.fold_left 
  (fun acc (name,value) ->
    if regex then (
      let incl = MapStr.fold (fun k _ acc -> k::acc) incl [] in
      let excl = MapStr.fold (fun k _ acc -> k::acc) excl [] in
      if List.length incl > 0 && (domatch incl name) = false then (
        acc
      ) else if List.length excl > 0 && (domatch excl name) = true then (
        acc
      ) else (
        f acc (name,value)
      )
    ) else (
      if (MapStr.is_empty incl) = false && (map_find incl name ) = None then (
        acc
      ) else if (MapStr.is_empty excl) = false && (map_find excl name) <> None then (
        acc
      ) else (
        f acc (name,value)
      )
    )
  ) init headers

(** check if internal or deleted message should only use index for this TBD **)
let should_include (email:Email.t) (record:mailbox_message_metadata) : bool =
  let internal =
  List.fold_left (fun acc (n,v) ->
    if (match_regex ~case:false n ~regx:"from") && 
        (match_regex ~case:false v ~regx:"mail system internal data <mailer-daemon@") then
      acc+1
    else if (match_regex ~case:false n ~regx:"subject") &&
        (match_regex ~case:false v ~regx:"DON'T DELETE THIS MESSAGE -- FOLDER INTERNAL DATA") then
      acc+1
    else
      acc
  ) 0 (Header.to_list (Email.header email)) in
  if internal = 2 
    (* deleted messages can be searched and fetched - tested in dovecot
      || (List.find record.flags ~f:(fun i -> if i = Flags_Deleted then true else
        false)) <> None*) then
    false
  else
    true

(** remove some headers from the message (it looks like dovecot removes
 * status, x-keywords,content-length,x-imapbase,x-uid
 **)
let prune_headers_list (headers:Header.t) : (string*string) list =
  fold_email_headers ~regex:true ~excl:(map_of_alist ["status";"x-status";"x-keywords";
  "content-length";"x-imapbase";"x-imap";"x-uid"]) headers
  ~init:[]
  ~f:(fun acc (name,value) -> (name,value) :: acc)

(** remove some headers from the message (it looks like dovecot removes
 * status, x-keywords,content-length,x-imapbase,x-uid
 **)
let prune_headers (headers:Header.t) : (Header.t) =
  let headers = prune_headers_list headers in
  Header.of_rev_list headers

let email_content_to_str (email:Email.t) : (string * int )=
  (*
  match Email.raw_content email with
  | None -> printf "####### raw_content is none\n%!"; ("",0)
  | Some cont -> 
      let str = (Octet_stream.to_string cont) in (str,String.length str)
  *)
  let str = raw_content email in (str, String.length str)

let email_headers_to_str ?(incl=MapStr.empty) ?(excl=MapStr.empty)
?(regex=false) (headers:Header.t) : (string*int) =
  let str =
  fold_email_headers ~excl ~incl ~regex headers
  ~init:""
  ~f:(fun acc (name,value) ->
    let header = name ^ ":" ^ value in
    if acc = "" then
      header
    else 
      acc ^ crlf ^ header
  ) in
  let str = str ^ crlf ^ crlf in
  (str,String.length str)

let email_headers_pruned_to_str (headers:Header.t) : (string*int) =
 email_headers_to_str ~regex:true ~excl:(map_of_alist ["status";"x-status";"x-keywords";
  "content-length";"x-imapbase";"x-imap";"x-uid"]) headers

(** get crlf'ed message and the message size **)
let email_to_str (email:Email.t) : (string*int) =
  let (headers,_) = email_headers_pruned_to_str (Email.header email) in
  let (content,_) = email_content_to_str email in
  let str = headers ^ content in
  (str,String.length str)

let find_header ?(default="") headers (key:string) : (string) =
  let value = map_find headers key in
  match value with
  | None -> default
  | Some value -> quote value

let get_nil_header headers (key:string) : string =
  find_header ~default:"NIL" headers key

let get_blnk_header headers (key:string) : string =
  find_header headers key

(** build envelope structure for the address:
jdoe@domain.com
**)
let get_addr (addr:string) : string =
  if match_regex addr ~regx:(all_of_it ( (group  "[^@]+") ^ "@" ^ (group ".+") )) then (
    let mbox = Str.matched_group 1 addr in
    let host = Str.matched_group 2 addr in
    "NIL" ^ space ^ (quote mbox) ^ space ^  (quote host)
  ) else ( (** ?? **)
    "NIL" ^ space ^ (quote addr) ^ space ^ (quote "")
  ) 

(** build envelope structure for the mailbox address:
Jon Doe <jdoe@domain.com>
**)
let get_mbox_addr (addr:string) : (string) =
  if match_regex addr ~regx:(all_of_it ( (optional "[^<>]+") ^ "<" ^ (group "[^<>]+") ^ ">")) then
    let disp = try Str.matched_group 1 addr with _ -> "" in
    let addr = Str.matched_group 2 addr in
    dlist_of (( quote  (replace ~regx:" " ~tmpl:"" disp) ) ^ space ^ (get_addr  addr) )
  else
    dlist_of ( (quote "")  ^ space ^ (get_addr addr) )

(** build envelope structure for the group list **)
let get_mbox_list (group:string) (addr:string) : (string) =
  let lmbx = Str.split (Str.regexp ",") addr in
  dlist_of ((quote "") ^ space ^ (quote "") ^ space ^ (quote group) ^ space ^ (quote "NIL")) ^
  (List.fold_left (fun acc i -> 
    if acc = "" then
      get_mbox_addr i
    else 
      acc ^ space ^ (get_mbox_addr i)
  ) "" lmbx) ^
  dlist_of ((quote "") ^ space ^ (quote "") ^ space ^ (quote "NIL") ^ space ^ (quote "NIL"))

(** build envelope structure for the address fields **)
let rec get_address headers (key:string) : string =
  let value = map_find headers key in
  match value with
  | None -> 
    if key = "sender" || key = "reply-to" then
      get_address headers "from"
    else
      "NIL"
  | Some addr ->
    let i = match_regex_i addr ~regx:":" in
    if i > 0 then ( (** group ? **)
      let group = Str.string_before addr i in
      let mbox_list = Str.string_after addr i in
      get_mbox_list group mbox_list 
    ) else (
      get_mbox_addr addr
    )

(** fetch the flags **)
let exec_fetch_flags (flags:mailboxFlags list) : (string) =
  let flags = flags_to_string flags in
  "FLAGS" ^ space ^ (list_of flags) 

(** fetch internal date **)
let exec_fetch_internaldate (date:Dates.ImapTime.t): (string) =
  "INTERNALDATE" ^ space ^ (quote (date_time_to_email date))

(** fetch internal date **)
let exec_fetch_modseq (modseq:int64): (string) =
  "MODSEQ" ^ space ^ (to_plist (Int64.to_string modseq))

(** fetch rfc822 message **)
let exec_fetch_rfc822 (email:Email.t) : string =
  printf "%s\n%!" (Email.to_string email);
  let (str,length) = email_to_str email in
  "RFC822 {" ^ (string_of_int length) ^ "}" ^ crlf ^ str

(** fetch rfc822 header **)
let exec_fetch_rfc822header (email:Email.t) : string =
  printf "%s\n%!" (Email.to_string email);
  let (str,length) = email_headers_pruned_to_str (Email.header email) in
  "RFC822.HEADER {" ^ (string_of_int length) ^ "}" ^ crlf ^ str

(** fetch rfc822 text **)
let exec_fetch_rfc822text (email:Email.t) : string =
  printf "%s\n%!" (Email.to_string email);
  let (str,length) = email_content_to_str (email) in
  "RFC822.TEXT {" ^ (string_of_int length) ^ "}" ^ crlf ^ str

(** fetch rfc822 text **)
let exec_fetch_rfc822size (email:Email.t) : string =
  printf "%s\n%!" (Email.to_string email);
  let (_,length) = email_to_str email in
  "RFC822.SIZE " ^ (string_of_int length) 
  
(** get seq_number from the string **)
let get_seq_number_exn (number:string) : (seq_number) =
  if number = "*" then
    Wild
  else if match_regex number ~regx:"^[1-9][0-9]*$" then
    Number (int_of_string number)
  else
    raise InvalidSequence

(** get set_set structure from seq-number [":" seq-number]? **)
let get_seq_set_exn (str_set:string) : (seq_set) =
  let num_list = Str.split (Str.regexp ":") str_set in
  let len = List.length num_list in
  if ( len > 0 && len <= 2) = false then
    raise InvalidSequence
  else if len = 1 then
    let n = get_seq_number_exn (List.nth num_list 0) in
    SeqNumber (n)
  else
    let n1 = get_seq_number_exn (List.nth num_list 0) in
    let n2 = get_seq_number_exn (List.nth num_list 1) in
    SeqRange (n1,n2)


(** parse sequence set into occaml structure for execution 
 seq-number = nz-number|"*"
 seq-range = seq-number ":" seq-number
 sequence-set = (seq-number | seq-range) ["," sequence-set]*
 **)
let get_sequence (sequence:string) : ( seq_set list) =
  let lofset = Str.split (Str.regexp ",") sequence in
  List.fold_left (fun acc range ->
    let r = get_seq_set_exn range in r :: acc) [] lofset

let match_seq_num (seq_num:seq_number) (num:int) : (bool) =
  match seq_num with
  | Wild -> true
  | Number i -> i = num

let match_seq_range (seq_num1:seq_number) (seq_num2:seq_number) (num:int) : (bool) =
  match seq_num1 with
  | Wild ->
      (match seq_num2 with
      | Wild -> true
      | Number i -> num <= i
      )
  | Number i1 ->
      match seq_num2 with
      | Wild -> num >= i1
      | Number i2 -> num >= i1 && num <= i2

let match_seq (seq:seq_set) (num:int) : (bool) =
  match seq with
  | SeqNumber n -> match_seq_num n num
  | SeqRange (n1,n2) -> match_seq_range n1 n2 num

(** match message to the sequence set 
if msg_seq is none then match to message UID instead of the seq
**)
let exec_seq (seqset:sequence) (msg_seq:int) : (bool) =
  list_find seqset (fun seq -> if match_seq seq msg_seq then true else false)
  
(** match the flag
 * need the index data for this, the headers don't have any flags
 **)
let exec_flag (flags:mailboxFlags list) (flag:searchFlags) : (bool) =
  let find_flag flags fl =
    list_find flags (fun f -> f = fl)
  in
  match flag with
    | Common flag -> find_flag flags flag
    | NotCommon flag -> find_flag flags flag = false
    | Old -> find_flag flags Flags_Recent
    | New -> (find_flag flags Flags_Recent) && (find_flag flags Flags_Seen) = false

(** match the date **)
let exec_hdr_date headers (date:Dates.ImapDate.t) (op:int->int->bool) : (bool) = 
  let value = map_find headers "date" in
  match value with 
  | None -> false
  | Some value -> 
    let tm = email_to_date_time_exn value in
    let diff = Dates.ImapDate.compare date (Dates.ImapTime.to_date tm) in
    op diff 0

(** match the internal date **)
let exec_date (internal:Dates.ImapTime.t) (date:Dates.ImapDate.t) (op:int->int->bool) : (bool) = 
  let idate = Dates.ImapTime.to_date internal in
  op (Dates.ImapDate.compare idate date) 0

(** match header field **)
let exec_hdr headers (header:string) (value:string) : (bool) = 
  let field = map_find headers (String.lowercase header) in
  match field with
  | None -> false
  | Some field -> match_regex field ~regx:value

let exec_text headers (content:string) (text:string) :
  (bool) =
  match_regex content ~regx:text || 
  let data = MapStr.fold (fun _ v acc -> v::acc) headers [] in
  (list_find data (fun i -> if match_regex i ~regx:text then
    true else false))

(** execute one key **)
let exec_one_search_key headers content (record:mailbox_message_metadata) seq key =
  match key with
  | Search_All -> true 
  | Search_Answered -> exec_flag record.flags (Common Flags_Answered)
  | Search_Bcc text -> exec_hdr headers "bcc" text
  | Search_Before date -> exec_date record.internal_date date (<)
  | Search_Body text -> match_regex content text
  | Search_Cc text -> exec_hdr headers "cc" text
  | Search_Deleted -> exec_flag record.flags (Common Flags_Deleted)
  | Search_Draft -> exec_flag record.flags (Common Flags_Draft)
  | Search_Flagged -> exec_flag record.flags (Common Flags_Flagged)
  | Search_From text -> exec_hdr headers "from" text
  | Search_Header (header, name) -> exec_hdr headers header name
  | Search_Keyword k -> exec_flag record.flags (Common (Flags_Keyword k))
  | Search_Larger size -> record.size > size
  | Search_Modseq (_,modseq) -> Int64.compare record.modseq modseq > 0(* ignoring entry and type *)
  | Search_New -> exec_flag record.flags (New)
  | Search_Old -> exec_flag record.flags (Old)
  | Search_On date -> exec_date record.internal_date date (=)
  | Search_Recent -> exec_flag record.flags (Common Flags_Recent)
  | Search_Seen -> exec_flag record.flags (Common Flags_Seen)
  | Search_Sentbefore date -> exec_hdr_date headers date (<) 
  | Search_Senton date -> exec_hdr_date headers date (=) 
  | Search_Sentsince date -> exec_hdr_date headers date (>=)
  | Search_SeqSet sequence -> exec_seq sequence seq
  | Search_Since date -> exec_date record.internal_date date (>=)
  | Search_Smaller size -> record.size < size
  | Search_Subject text -> exec_hdr headers "subject" text
  | Search_Text text -> exec_text headers content text
  | Search_To text -> exec_hdr headers "to" text
  | Search_UID seq -> exec_seq seq record.uid
  | Search_Unanswered -> exec_flag record.flags (NotCommon Flags_Answered)
  | Search_Undeleted -> exec_flag record.flags (NotCommon Flags_Deleted)
  | Search_Undraft -> exec_flag record.flags (NotCommon Flags_Draft)
  | Search_Unflagged -> exec_flag record.flags (NotCommon Flags_Flagged)
  | Search_Unkeyword k -> exec_flag record.flags (NotCommon (Flags_Keyword k))
  | Search_Unseen -> exec_flag record.flags (NotCommon Flags_Seen)

let exec_seq_or_uid seq uid key =
  match key with
  | Search_SeqSet sequence -> exec_seq sequence seq
  | Search_UID sequence -> exec_seq sequence uid
  | _ -> true

(** execute all search keys, result is and'ed
**)
let rec _exec_search_all search_one keys =
  match keys with
  | Key k -> search_one k
  | KeyList kl ->
    (try
      List.fold_left 
      (fun acc k -> 
        if acc && (_exec_search_all search_one k) = false then
          raise ExecDone
        else
          true
      ) true kl
    with ExecDone -> false
    )
  | OrKey (k1,k2) ->
      let res1 = _exec_search_all search_one k1 in
      if res1 then
        true
      else
        _exec_search_all search_one k2 
  | NotKey k -> (_exec_search_all search_one k ) = false

let exec_search_all headers content keys record seq =
  _exec_search_all (exec_one_search_key headers content record seq) keys

(* traverse the keys for a key *)
let rec has_key keys f =
  try
  match keys with
  | Key k -> f k
  | KeyList kl ->
    List.fold_left (fun acc k ->
      if has_key k f then
        raise ExecDone
      else
        false
    ) false kl
  | OrKey (k1,k2) -> 
    if has_key k1 f || has_key k2 f then
      raise ExecDone
    else
      false
  | NotKey k -> has_key k f
  with ExecDone -> true

(* check if search fails just because of sequence of uid -
 * don't need to read the record then
 *)
let check_search_seq keys ~seq ~uid =
  _exec_search_all (exec_seq_or_uid seq uid) keys

(** if uid is some then msg seq#, otherwise uid should be extracted from the
 * headers. If search result true then need to either return the seq or the uid
 * option from the header
**)
let exec_search (email:Email.t) (keys:(searchKey) searchKeys) (** encapsulate email in
functor or another module to hide implementation TBD **)
(record:mailbox_message_metadata) (seq:int) : (bool) =
  if should_include email record = false then
    false
  else
    let hl = Header.to_list (Email.header email) in
    let headers = List.fold_left
    (fun map (k,v) -> MapStr.add (String.lowercase k) v map) MapStr.empty hl in
    let (cont,_) = email_content_to_str email in
    exec_search_all headers cont keys record seq 

(** don't format the envelope **)
let exec_fetch_envelope_unf headers : (string) =
  let envelope = 
    [ get_nil_header headers "date" ;
      get_nil_header headers "subject";
      get_address headers "from";
      get_address headers "sender";
      get_address headers "reply-to";
      get_address headers "to";
      get_address headers "cc";
      get_address headers "bcc";
      get_nil_header headers "in-reply-to";
      get_nil_header headers "message-id";
    ] in
  List.fold_left (fun acc i -> if acc = "" then i
      else acc ^ space ^ i) "" envelope

(** fetch envelope **)
let exec_fetch_envelope headers : (string) =
  let env = exec_fetch_envelope_unf headers in
  "ENVELOPE (" ^ env ^ "))"

let exec_fetch_uid (record:mailbox_message_metadata) : string =
  "UID " ^ (string_of_int record.uid)

(** 4.3.2.1. **)
let section_part_str (l:int list) : string =
  let str =
  List.fold_left
  (fun acc i -> 
    let i = string_of_int i in 
    if acc = "" then 
      i 
    else 
      acc ^ "." ^ i
  ) "" l in
  if str <> "" then
    str ^ "."
  else
    str

(** <x> {xxx},substr **)
let body_part_str (l:int list) (str:string) : (string*string) =
  let (part,str) =
  if List.length l = 0 then
    "",str
  else (
    let start = List.nth l 0 in
    let part = ang_list_of (string_of_int start) in
    let size = if (List.length l) = 2 then Some (List.nth l 1) else None in
    let str = Utils.substr str start size in
    part, str
  )
  in
    part ^ " {" ^ (string_of_int (String.length str)) ^ "}", str

let exec_fetch_header ?(incl=MapStr.empty) ?(excl=MapStr.empty) ?(regex=false)
(email:Email.t) : string =
  printf "exec_fetch_header %d %d %b\n%!" (MapStr.cardinal incl)
  (MapStr.cardinal excl) regex;
  let str,_ = email_headers_to_str ~incl ~excl ~regex (Email.header email) in str

let exec_fetch_text (email:Email.t) : string =
  (*let cont = Email.raw_content email in
  match cont with 
  | None -> printf "####### raw_content is none\n%!"; ""
  | Some cont -> Octet_stream.to_string cont
  *)
  raw_content email

(** build body sectin reply string **)
let body_template_str (prefix:string) (content:string) (sec:sectionPart)
(part:bodyPart) : string =
  let (part,str) = body_part_str part content in
  let sec = section_part_str sec in
  "BODY[" ^ sec ^ prefix ^ "]" ^ part ^ crlf ^ str
  
let mk_headers_fetch_str prefix headers =
  prefix ^ " " ^
  (List.fold_left (fun acc h -> 
    if acc = "(" then
      acc ^ String.uppercase h
    else 
      acc ^ " " ^ (String.uppercase h)
  ) "(" headers) ^ ")"

let exec_fetch_msgtext (email:Email.t) (msgtext:sectionMsgtext) (sec:sectionPart) (part:bodyPart): string =
  let (prefix,str) =
  (match msgtext with
  | Header -> ("HEADER", exec_fetch_header ~regex:true email )
  | HeaderFields incl -> 
    (mk_headers_fetch_str "HEADER.FIELDS" incl,
      exec_fetch_header ~regex:true ~incl:(map_of_alist incl) email )
  | HeaderFieldsNot excl -> 
    (mk_headers_fetch_str "HEADER.FIELDS.NOT" excl,
      exec_fetch_header ~regex:true ~excl:(map_of_alist excl) email )
  | Text -> ("TEXT",exec_fetch_text email )
  ) in
  body_template_str prefix str sec part

let exec_fetch_mime (email:Email.t) (sec:sectionPart) (part:bodyPart) : string = 
  let incl = MapStr.empty in
  let incl = MapStr.add "^content-" () incl in
  let str = exec_fetch_header ~regex:true ~incl email in
  body_template_str "MIME" str sec part

let exec_fetch_email_body (email:Email.t) (sec:sectionPart) (part:bodyPart) : string =
  let (str,_) = email_to_str email in
  body_template_str "" str sec part

exception SectionDone
(** find the requested section, return empty email if not found **)
let find_fetch_section (email:Email.t) (secPart:sectionPart) : Email.t =
  try
    List.fold_left (fun email part ->
      printf "find_fetch_section %d type: %s\n%!" part (mime_type email);
      if part = 0 then
        email
      else (
        let content = Email.content email in
        match content with
        | `Data cont -> printf "find_fetch_section data\n%!";raise SectionDone
        | `Message email -> printf "find_fetch_section message\n%!";raise SectionDone
        | `Multipart lemail -> printf "find_fetch_section multipart >= \n%!";
          if part >= (List.length lemail) then
            raise SectionDone
          else
            List.nth lemail part
      )
    ) email secPart
  with SectionDone -> printf "find_fetch_section not found\n%!"; Email.empty()

let exec_fetch_sectext (email:Email.t) (secPart:sectionPart) (secText:sectionText option)
(spec:sectionSpec) (part:bodyPart) =
  let email = find_fetch_section email secPart in
  match secText with 
  | None -> exec_fetch_email_body email secPart part
  | Some secText ->
      match secText with
      | SectionMsgtext msgtext -> exec_fetch_msgtext email msgtext secPart part
      | Mime -> exec_fetch_mime email secPart part

let exec_fetch_body_section (email:Email.t)
(record:mailbox_message_metadata) (spec:sectionSpec) (part:bodyPart) : string =
  match spec with
  | SectionMsgtext msgtext -> 
      (
      match msgtext with 
      |None -> exec_fetch_email_body email [] part
      |Some msgtext -> exec_fetch_msgtext email msgtext [] part
      )
  | SectionPart (secPart,secText) -> exec_fetch_sectext email secPart secText spec part

let get_params (str:string) : string =
  let params = 
  try
    let l = Str.split (Str.regexp "[ ]*;[ ]*") str in
    printf "%d\n" (List.length l);
    List.fold_left (fun acc nv ->
      if match_regex nv ~regx:"^[ ]*\\([^= ]+\\)[ ]*=[ ]*\\([^ ]+\\|\"[^\"]+\"\\)[ ]*$" then (
        let name = Str.matched_group 1 nv in
        let value = Str.matched_group 2 nv in
        let nv = (quote name) ^ " " ^ (quote value) in
        if acc = "NIL" then
          nv
        else
          acc ^ " " ^ nv
      ) else ( (** why TBD **)
        acc
      )
    ) "NIL" l
  with _ -> "NIL"
  in
  params

(** body type/subtype and parameters
text/plain ; charset=us-ascii
message/rfc822 ; name="Re: thread test.eml"
**)
let fetch_type_and_param (email:Email.t) : (string*string*string) =
  let str,_ = email_headers_to_str (Email.header email) in
  printf "fetch_type_and_param %s\n%!" str;
  let media = media_type email in
  let t = (Media_type.mime_type media) in
  let st = (Media_type.mime_subtype media) in
  let l = Media_type.params media in
  let l = Field_name.Assoc.to_list l in
  let def = (quote "charset") ^ " " ^ (quote "us-ascii") in
  let params = List.fold_left
  (fun acc (n,v) -> 
    let str = (quote n) ^ " " ^ (quote v) in
    if acc = def then
      str
    else
      acc ^ " " ^ str
  ) def l in
  (quote t,quote st,list_of params)

(** fetch encoding **)
let fetch_enc (email:Email.t): string =
  try
    let el = Header.Content_transfer_encoding.all (Email.header email) in
    let enc = List.nth el 0 in
    quote (Encoding.to_string enc)
  with _ -> "NIL"

let fetch_id_and_descr (email:Email.t) : string*string =
  let hm = fold_email_headers ~incl:(map_of_alist
  ["content-id";"content-description"]) ~regex:true (Email.header email)
  ~init:MapStr.empty
  ~f:(fun acc (n,v) -> 
    if match_regex n ~regx:"id" then
      MapStr.add "id" (quote v) acc
    else
      MapStr.add "descr" (quote v) acc
  ) in
  let id = try MapStr.find "id" hm with _ -> "NIL" in
  let descr = try MapStr.find "descr" hm with _ -> "NIL" in
  (id,descr)

let fetch_size (email:Email.t) : string * string =
  let (cont,size) = email_content_to_str email in
  let rec cntlines str i cnt =
    if i >= String.length str then
      cnt
    else if str.[i] = '\n' then
      cntlines str (i + 1) (cnt + 1)
    else
      cntlines str (i + 1) cnt
  in
  let lines = cntlines cont 0 0 in
  string_of_int size,string_of_int lines

(**
Content-Disposition: inline;
        filename=rose.jpg
Content-Disposition: attachment;
        filename="Re: thread test.eml"
("attachment" ("filename" "Re: thread test.eml")
("inline" ("filename" "rose.jpg"))
perhaps should use Email_message Media_type to get this TBD **)
let fetch_disposition (header:Header.t) : string =
  let headers = headers_to_map header in
  let disp = try MapStr.find "content-disposition" headers with _ -> "NIL" in
  if disp = "NIL" then
    disp
  else (
    let disp = replace ~regx:"\r" ~tmpl:"" disp in
    let disp = replace ~regx:"\n" ~tmpl:"" disp in
    if match_regex disp ~regx:"^[ \t]*\\([^; \t]+\\)\\([ \t]*;[ \t]*\\(.+\\)\\)$" then
      let name = Str.matched_group 1 disp in
      let params = Str.matched_group 3 disp in
      let params = get_params params in
      list_of ((quote name) ^ " " ^ (list_of params))
    else
      list_of (quote disp)
  )

(** some bodystructure fields **)
let fetch_bodystructure_fields (email:Email.t) : bodystr_fields =
  let (t,st,params) = fetch_type_and_param email in
  let (id,descr) = fetch_id_and_descr email in
  let enc = fetch_enc email in
  let (size,lines) = fetch_size email in
  let disp = fetch_disposition (Email.header email) in
  {btype = t; bsubtype = st; bparams = params; bid = id; bdescr = descr; benc =
    enc; bsize = size; blines = lines;bdisp=disp}

let format_basic (basic:bodystr_fields) : string =
  String.concat " "
  [basic.btype;basic.bsubtype;basic.bparams;basic.bid;basic.bdescr;basic.benc;basic.bsize;]

let format_simple (basic:bodystr_fields) (body:bool) : string =
  let main =
    let l = [basic.btype;basic.bsubtype;basic.bparams;basic.bid;basic.bdescr;basic.benc;basic.bsize;] in
    if basic.btype = "\"text\"" then
      List.concat [l;[basic.blines]]
    else
      l
  in
  (** md5;body-disposition;body-language;body-location TBD **)
  let extension =
    if body = false then
      ["NIL";basic.bdisp;"NIL";"NIL"]
    else
      []
  in
  printf "format_simple %b %d\n%!" body (List.length extension);
  let all = List.concat [main;extension] in
  String.concat " " all

let fetch_message_bodystructure (email:Email.t) (basic:bodystr_fields) (body:bool)
  (bodystructure_folder:Email.t->bool->string) : string =
  printf "fetch_message_bodystructure\n%!";
  let main =(format_basic basic) ^ " " ^
  (list_of (exec_fetch_envelope_unf (headers_to_map (Email.header email)))) ^ 
  (bodystructure_folder email body) ^ " " ^ basic.blines in
  (** md5, disposition, language, location **)
  let all =
    if body = false then
      let extension =  "NIL" ^ " " ^ basic.bdisp ^ " " ^ "NIL" ^ " " ^ "NIL" in
      main ^ " " ^ extension
    else
      main
  in
  list_of (all)

let fetch_simple_bodystructure (email:Email.t) (basic:bodystr_fields) (body:bool) : string =
  printf "fetch_text_or_basic_bodystructure %s\n%!" basic.btype;
  list_of (format_simple basic body)

let fetch_multipart_bodystructure (lemail:Email.t list) (basic:bodystr_fields) (body:bool)
  (bodystructure_folder:Email.t->bool->string) : string =
  printf "fetch_multipart_bodystructure %s\n%!" basic.btype;
  let main = (List.fold_left
  (fun acc email ->
    acc ^ (bodystructure_folder email body) 
  ) "" lemail) ^ " " ^ basic.bsubtype in
  (** params, disposition, language, location **)
  let all =
  if body = false then 
    let extension = basic.bparams ^ " " ^ basic.bdisp ^ " " ^ "NIL" ^ " " ^ "NIL" in
    main ^ " " ^ extension
  else
    main
  in
  list_of all

let rec fold_email_bodystructure (email:Email.t) (body:bool): string =
  let basic = fetch_bodystructure_fields email in
  let cont = Email.content email in
  match cont with
  | `Data cont -> 
    (match get_message_email email cont with
    | Some email -> fetch_message_bodystructure email basic body fold_email_bodystructure
    | None -> fetch_simple_bodystructure email basic body
    )
  | `Message email -> printf "fold_email_bodystructure message %s\n%!" basic.btype; 
    fetch_message_bodystructure email basic body fold_email_bodystructure
  | `Multipart lemail -> fetch_multipart_bodystructure lemail basic body fold_email_bodystructure

(** bodyscture for the whole email-message **)
let exec_fetch_bodystructure (email:Email.t) : string =
  let str = fold_email_bodystructure email false in
  "BODYSTRUCTURE " ^ str

(** bodyscture for the whole email-message **)
let exec_fetch_body (email:Email.t) : string =
  let str = fold_email_bodystructure email true in
  "BODY " ^ str

let exec_fetch_att (seq:int) (sequence:sequence) (email:Email.t) 
(record:mailbox_message_metadata) (att:fetchAtt list) : (string) = 
  let headers = headers_to_map (Email.header email) in
  List.fold_left
  (fun acc item ->
    let res =
    (match item with
    | Fetch_Body -> exec_fetch_body email
    (** TBD set \Seen **)
    | Fetch_BodySection (spec,part) -> exec_fetch_body_section email record spec part
    (** TBD no setting \Seen **)
    | Fetch_BodyPeekSection (spec,part) -> exec_fetch_body_section email record spec part
    | Fetch_Bodystructure -> exec_fetch_bodystructure email
    | Fetch_Envelope -> exec_fetch_envelope headers
    | Fetch_Flags -> exec_fetch_flags record.flags
    | Fetch_Modseq -> exec_fetch_modseq record.modseq
    | Fetch_Internaldate -> exec_fetch_internaldate record.internal_date
    | Fetch_Rfc822 -> exec_fetch_rfc822 email
    | Fetch_Rfc822Header -> exec_fetch_rfc822header email
    | Fetch_Rfc822Size -> exec_fetch_rfc822size email
    | Fetch_Rfc822Text -> exec_fetch_rfc822text email
    | Fetch_Uid -> exec_fetch_uid record) in
    if acc = "" then
      res
    else
      acc ^ space ^ res
  ) "" att

let exec_fetch_macro (seq:int) (sequence:sequence) (email:Email.t)
 (record:mailbox_message_metadata) (macro:fetchMacro) : (string) =
  match macro with
  | Fetch_All -> exec_fetch_att seq sequence email record
    [Fetch_Flags;Fetch_Internaldate;Fetch_Rfc822Size;Fetch_Envelope]
  | Fetch_Fast -> exec_fetch_att seq sequence email record
    [Fetch_Flags;Fetch_Internaldate;Fetch_Rfc822Size]
  | Fetch_Full -> exec_fetch_att seq sequence email record
    [Fetch_Flags;Fetch_Internaldate;Fetch_Rfc822Size;Fetch_Envelope;Fetch_Body]
      

let exec_fetch_all (seq:int) (sequence:sequence)
(email:Email.t) (record:mailbox_message_metadata) (fetchattr:fetch) (buid:bool) : string =
  let open Email_message.Mailbox.Message in
  match fetchattr with
  | FetchMacro macro -> exec_fetch_macro seq sequence email record macro
  | FetchAtt att -> exec_fetch_att seq sequence email record att

(** need to trim based on seq TBD **)
let exec_fetch (seq:int) (sequence:sequence) (message:Mailbox.Message.t)
(record:mailbox_message_metadata) (attr:fetch) (changedsince:int64 option) (buid:bool) : string option =
  let open Email_message.Mailbox.Message in
  if should_include message.email record = false then
    None
  else if (buid = false && exec_seq sequence seq || buid = true && (exec_seq sequence record.uid))  &&
    (changedsince = None || Int64.compare record.modseq (option_value_exn
    changedsince) > 0) then (
      let str = exec_fetch_all seq sequence message.email record attr buid in
      let seq = string_of_int seq in
      Some (seq ^ space ^ "FETCH" ^ space ^ (list_of str))
  ) else
    None

let join_flags (flags1:mailboxFlags list) (flags2:mailboxFlags list) : (mailboxFlags list) =
  let l = List.concat [flags1;flags2] in
  let (_,l) = List.fold_right (fun a (prev,acc) -> 
    match prev with 
    |None->Some a,a::acc
    |Some prev when prev = a -> Some a,acc
    |Some prev->Some a,a::acc) (List.sort Pervasives.compare l) (None,[])
  in
  l

let rem_flags (flags:mailboxFlags list) (rem:mailboxFlags list) : (mailboxFlags list) =
  List.filter (fun fl -> (list_find rem (fun fl1 -> fl1 = fl)) = false) flags

(*
 * * 19 FETCH (UID 128 FLAGS (\Seen $NotJunk NotJunk))
 * * 20 FETCH (UID 129 FLAGS (\Seen $NotJunk NotJunk))
 * a OK Fetch completed.
 * a fetch 1:* flags
 * * 1 FETCH (FLAGS (\Seen unchangedsince 10000000000))
 * * 2 FETCH (FLAGS (\Seen $NotJunk NotJunk))
 * * 3 FETCH (FLAGS (\Seen $NotJunk NotJunk))
 *
 * * OK [HIGHESTMODSEQ 491] Highest
 * * 1 FETCH (UID 47 MODSEQ (494) FLAGS ($NotJunk NotJunk))
 * * 2 FETCH (UID 48 MODSEQ (494) FLAGS ($NotJunk NotJunk))
 * * 3 FETCH (UID 49 MODSEQ (494) FLAGS ($NotJunk NotJunk))
 *
 * * 1 FETCH (MODSEQ (496) FLAGS (\Seen $NotJunk NotJunk))
 * * 2 FETCH (MODSEQ (496) FLAGS (\Seen $NotJunk NotJunk))
 * * 3 FETCH (MODSEQ (496) FLAGS (\Seen $NotJunk NotJunk))
 *)
let format_flags seq modseq buid (record:mailbox_message_metadata) =
  let flags = flags_to_string record.flags in
  let seq = string_of_int seq in
  let uid = 
    if buid then 
      "UID " ^ (string_of_int record.uid) ^ space
    else
      ""
  in
  let modseq = 
    if modseq then
      "MODSEQ (" ^ (Int64.to_string record.modseq) ^ ") " 
    else
      ""
  in
  seq ^ space ^ "FETCH (" ^ uid ^ modseq ^ "FLAGS (" ^ flags ^ ")"

(* send response for silent even if modseq is on
 * don't send response if the flag update
 * doesn't change the original set of flags
 *)
let get_flags seq modseq buid flags record =
  let flags_eq flags record =
    if List.length flags <> List.length record.flags then
      false
    else (
      let cmp f1 f2 = if f1 > f2 then 1 else if f1 < f2 then -1 else 0 in
      let flags1 = List.sort cmp flags in
      let flags2 = List.sort cmp record.flags in
      List.for_all2 (fun e1 e2 -> e1 = e2) flags1 flags2
    )
  in
  match flags with
  | `Silent flags -> 
    if flags_eq flags record = true then
      `None
    else if modseq then (
      let record = {record with flags} in
      `Ok (record, format_flags seq modseq buid record)
    ) else
      `Silent {record with flags}
  | `Ok flags -> 
    if flags_eq flags record = true then
      `None
    else (
      let record = {record with flags} in
      `Ok (record, format_flags seq modseq buid record)
    )

let exec_store_flags (record:mailbox_message_metadata) (seq:int) (storeattr:storeFlags)
 (flagsval:mailboxFlags list) (modeseq:bool) (buid:bool): 
   [> `Ok of mailbox_message_metadata*string|`Silent of mailbox_message_metadata|`None] =
  let flags =
  match storeattr with
  | Store_Flags -> `Ok flagsval 
  | Store_FlagsSilent -> `Silent flagsval
  | Store_PlusFlags -> `Ok (join_flags record.flags flagsval)
  | Store_PlusFlagsSilent ->`Silent (join_flags record.flags flagsval)
  | Store_MinusFlags -> `Ok (rem_flags record.flags flagsval)
  | Store_MinusFlagsSilent -> `Silent (rem_flags record.flags flagsval)
  in
  get_flags seq modeseq buid flags record

let exec_store (record:mailbox_message_metadata) (seq:int) (sequence:sequence)
(storeattr:storeFlags) (flagsval:mailboxFlags list) (unchangedsince:Int64.t option) (buid:bool) :
  [`Ok of mailbox_message_metadata*string|`Silent of mailbox_message_metadata|`Modseqfailed of int|`None] =
  if buid = false && exec_seq sequence seq || buid = true && (exec_seq sequence record.uid)  then (
    if unchangedsince = None || Int64.compare record.modseq (option_value_exn unchangedsince) <= 0 then
      exec_store_flags record seq storeattr flagsval (unchangedsince <> None) buid
    else
      `Modseqfailed (if buid then record.uid else seq)
  ) else 
    `None
