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
open Lwt
open Imaplet_types
open Regex
open Storage_meta
open Utils
open Dates
open Lazy_message

exception InvalidSequence

exception ExecDone

exception InvalidMessage

module MapStr = Map.Make(String)

let find_header headers name =
  let tol s = String.lowercase s in
  try 
    let (_,v) = List.find (fun (n,_) -> (tol n) = (tol name)) headers in (Some v)
  with Not_found -> None

let find_header_value headers name value =
  let field = find_header headers (String.lowercase name) in
  match field with
  | None -> false
  | Some field -> match_regex ~case:false field ~regx:value

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

let default_bodystr_fields =
  {
    btype = "\"text\"";
    bsubtype = "\"plain\"";
    bparams = "(\"charset\" \"us-ascii\")";
    bid = "NIL";
    bdescr = "NIL";
    benc = "NIL";
    bsize = "";
    blines = "";
    bdisp = "NIL";
  }

let flags_to_string flags =
  String.concat " " (List.map fl_to_str flags)

let map_of_alist (l:string list) =
  List.fold_left (fun m i -> MapStr.add i () m) MapStr.empty l

let exclude = map_of_alist
  ["status";"x-status";"x-keywords"; "content-length";"x-imapbase";"x-imap";"x-uid"]

(** get crlf'ed message and the message size **)
let email_to_str (module LE:LazyEmail_inst) =
  LE.LazyEmail.to_string ~excl:exclude LE.this >>= fun str ->
  return (str,String.length str)

let find_header_opt ?(default="") headers (key:string) : (string) =
  let value = find_header headers key in
  match value with
  | None -> default
  | Some value -> quote value

let get_nil_header headers (key:string) : string =
  find_header_opt ~default:"NIL" headers key

let get_blnk_header headers (key:string) : string =
  find_header_opt headers key

(** build envelope structure for the address:
jdoe@domain.com
**)
let addr_re = all_of_it ( String.concat "" [group  "[^@]+" ; "@" ; (group ".+")] )
let get_addr (addr:string) : string =
  if match_regex addr ~regx:addr_re then (
    let mbox = Str.matched_group 1 addr in
    let host = Str.matched_group 2 addr in
    String.concat space ["NIL" ; (quote mbox) ; (quote host)]
  ) else ( (** ?? **)
    String.concat space ["NIL" ; (quote addr) ; (quote "")]
  ) 

(** build envelope structure for the mailbox address:
Jon Doe <jdoe@domain.com>
**)
let mbox_addr_re = all_of_it ( String.concat "" [optional "[^<>]+" ; "<" ; group "[^<>]+" ; ">"])
let get_mbox_addr (addr:string) : (string) =
  if match_regex addr ~regx:mbox_addr_re then
    let disp = try Str.matched_group 1 addr with _ -> "" in
    let addr = Str.matched_group 2 addr in
    dlist_of (String.concat "" [quote  (replace ~regx:" " ~tmpl:"" disp) ; space ; get_addr  addr ])
  else
    dlist_of ( String.concat "" [(quote "")  ; space ; (get_addr addr) ])

(** build envelope structure for the group list **)
let get_mbox_list (group:string) (addr:string) : (string) =
  let lmbx = Str.split (Str.regexp ",") addr in
  String.concat "" [dlist_of (String.concat space [quote "" ; quote "" ; quote group ; quote "NIL"]) ;
  String.concat space (List.map get_mbox_addr lmbx) ;
  dlist_of (String.concat space [quote "" ; quote "" ; quote "NIL" ; quote "NIL"])]

(** build envelope structure for the address fields **)
let rec get_address headers key =
  let value = find_header headers key in
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
let exec_fetch_flags metadata =
  let flags = flags_to_string metadata.flags in
  return (String.concat "" ["FLAGS" ; space ; (list_of flags)]) 

(** fetch internal date **)
let exec_fetch_internaldate metadata =
  return (String.concat "" ["INTERNALDATE" ; space ; quote (date_time_to_email metadata.internal_date)])

(** fetch internal date **)
let exec_fetch_modseq metadata =
  return (String.concat "" ["MODSEQ" ; space ; to_plist (Int64.to_string metadata.modseq)])

(** fetch rfc822 message **)
let exec_fetch_rfc822 email =
  email_to_str email >>= fun (str,length) ->
  return (String.concat "" ["RFC822 {" ; string_of_int length ; "}" ; crlf ; str])

(** fetch rfc822 header **)
let exec_fetch_rfc822header (module LE:LazyEmail_inst) =
  let str = LE.LazyEmail.header_to_str ~excl:exclude LE.this in
  let length = String.length str in
  return (String.concat "" ["RFC822.HEADER {" ; string_of_int length ; "}" ; crlf ; str])

(** fetch rfc822 text **)
let exec_fetch_rfc822text (module LE:LazyEmail_inst) =
  LE.LazyEmail.raw_content LE.this >>= fun str ->
  return (String.concat "" ["RFC822.TEXT {" ; string_of_int (String.length str) ; "}" ; crlf ; str])

(** fetch rfc822 text **)
let exec_fetch_rfc822size (module LE:LazyEmail_inst) =
  let size = LE.LazyEmail.size LE.this in
  return ("RFC822.SIZE " ^ (string_of_int size))
  
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
let exec_flag (module LM:LazyMessage_inst) (flag:searchFlags) =
  LM.LazyMessage.get_message_metadata LM.this >>= fun meta ->
  let find_flag flags fl =
    list_find flags (fun f -> f = fl)
  in
  return (
  match flag with
    | Common flag -> find_flag meta.flags flag
    | NotCommon flag -> find_flag meta.flags flag = false
    | Old -> find_flag meta.flags Flags_Recent
    | New -> (find_flag meta.flags Flags_Recent) && (find_flag meta.flags Flags_Seen) = false
  )

let get_header (module LM:LazyMessage_inst) =
  LM.LazyMessage.get_email LM.this >>= fun (module LE:LazyEmail_inst) ->
  return (LE.LazyEmail.header LE.this)

(** match the date **)
let exec_hdr_date message (date:Dates.ImapDate.t) (op:int->int->bool) = 
  get_header message >>= fun headers ->
  let value = find_header headers "date" in
  return (
  match value with 
  | None -> false
  | Some value -> 
    let tm = email_to_date_time_exn value in
    let diff = Dates.ImapDate.compare date (Dates.ImapTime.to_date tm) in
    op diff 0
  )

(** match the internal date **)
let exec_date (module LM:LazyMessage_inst) (date:Dates.ImapDate.t) (op:int->int->bool) = 
  LM.LazyMessage.get_message_metadata LM.this >>= fun meta ->
  let idate = Dates.ImapTime.to_date meta.internal_date in
  return (op (Dates.ImapDate.compare idate date) 0)

(** match header field **)
let exec_hdr message name value = 
  get_header message >>= fun headers ->
  return (find_header_value headers name value)

let exec_body (module LM:LazyMessage_inst) text =
  LM.LazyMessage.get_content_block LM.this >>= fun content ->
  return (match_regex content ~regx:text)

let exec_text message text =
  get_header message >>= fun headers ->
  if (list_find headers (fun (_,value) -> match_regex ~case:false value ~regx:text)) then
    return true
  else 
    exec_body message text

let exec_size (module LM:LazyMessage_inst) op size =
  LM.LazyMessage.get_message_metadata LM.this >>= fun meta ->
  return (op meta.size size)

let exec_modseq (module LM:LazyMessage_inst) modseq =
  LM.LazyMessage.get_message_metadata LM.this >>= fun meta ->
  return (Int64.compare meta.modseq modseq > 0)

let exec_uid (module LM:LazyMessage_inst) seq =
  LM.LazyMessage.get_message_metadata LM.this >>= fun meta ->
  return (exec_seq seq meta.uid)

(** execute one key **)
let exec_one_search_key message seq key =
  match key with
  | Search_All -> return true 
  | Search_Answered -> exec_flag message (Common Flags_Answered)
  | Search_Bcc text -> exec_hdr message "bcc" text
  | Search_Before date -> exec_date message date (<)
  | Search_Body text -> exec_body message text
  | Search_Cc text -> exec_hdr message "cc" text
  | Search_Deleted -> exec_flag message (Common Flags_Deleted)
  | Search_Draft -> exec_flag message (Common Flags_Draft)
  | Search_Flagged -> exec_flag message (Common Flags_Flagged)
  | Search_From text -> exec_hdr message "from" text
  | Search_Header (header, name) -> exec_hdr message header name
  | Search_Keyword k -> exec_flag message (Common (Flags_Keyword k))
  | Search_Larger size -> exec_size message (>) size
  | Search_Modseq (_,modseq) -> exec_modseq message modseq (* ignoring entry and type *)
  | Search_New -> exec_flag message (New)
  | Search_Old -> exec_flag message (Old)
  | Search_On date -> exec_date message date (=)
  | Search_Recent -> exec_flag message (Common Flags_Recent)
  | Search_Seen -> exec_flag message (Common Flags_Seen)
  | Search_Sentbefore date -> exec_hdr_date message date (<) 
  | Search_Senton date -> exec_hdr_date message date (=) 
  | Search_Sentsince date -> exec_hdr_date message date (>=)
  | Search_SeqSet sequence -> return (exec_seq sequence seq)
  | Search_Since date -> exec_date message date (>=)
  | Search_Smaller size -> exec_size message (<) size
  | Search_Subject text -> exec_hdr message "subject" text
  | Search_Text text -> exec_text message text
  | Search_To text -> exec_hdr message "to" text
  | Search_UID seq -> exec_uid message seq
  | Search_Unanswered -> exec_flag message (NotCommon Flags_Answered)
  | Search_Undeleted -> exec_flag message (NotCommon Flags_Deleted)
  | Search_Undraft -> exec_flag message (NotCommon Flags_Draft)
  | Search_Unflagged -> exec_flag message (NotCommon Flags_Flagged)
  | Search_Unkeyword k -> exec_flag message (NotCommon (Flags_Keyword k))
  | Search_Unseen -> exec_flag message (NotCommon Flags_Seen)

let exec_seq_or_uid seq uid key =
  return (
  match key with
  | Search_SeqSet sequence -> exec_seq sequence seq
  | Search_UID sequence -> exec_seq sequence uid
  | _ -> true
  )

(** execute all search keys, result is and'ed
**)
let rec _exec_search_all search_one keys =
  match keys with
  | Key k -> search_one k
  | KeyList kl ->
    catch (fun () ->
      Lwt_list.fold_left_s 
      (fun acc k -> 
        if acc then (
          lwt res = _exec_search_all search_one k in
          if res = false then
            raise ExecDone
          else
            return true
        ) else
          return true
      ) true kl
    ) (function 
      |ExecDone -> return false
      |ex -> 
        Log_.log `Error (Printf.sprintf "### search error %s\n" (Printexc.to_string ex)); 
        return false
    )
  | OrKey (k1,k2) ->
      lwt res1 = _exec_search_all search_one k1 in
      if res1 then
        return true
      else
        _exec_search_all search_one k2 
  | NotKey k -> 
    lwt res = _exec_search_all search_one k in
    return (res = false)

let exec_search_all message keys seq =
  _exec_search_all (exec_one_search_key message seq) keys

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

(** if uid is some then msg seq#, otherwise uid should be extracted from the
 * headers. If search result true then need to either return the seq or the uid
 * option from the header
**)
let exec_search message (keys:(searchKey) searchKeys) (seq:int) =
  exec_search_all message keys seq 

(** don't format the envelope **)
let exec_fetch_envelope_unf headers =
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
  return (String.concat space envelope)

(** fetch envelope **)
let exec_fetch_envelope headers =
  exec_fetch_envelope_unf headers >>= fun env ->
  return (String.concat "" ["ENVELOPE (" ; env ; "))"])

let exec_fetch_uid metadata =
  return ("UID " ^ (string_of_int metadata.uid))

(** 4.3.2.1. **)
let section_part_str (l:int list) : string =
  String.concat "." (List.map string_of_int l)
  (* is this needed TBD
  in
  if str <> "" then
    str ^ "."
  else
    str
  *)

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
  String.concat "" [part ; " {" ; string_of_int (String.length str) ; "}"], str

let exec_fetch_header ?(incl=`Map MapStr.empty) ?(excl=MapStr.empty) (module LE:LazyEmail_inst) =
  LE.LazyEmail.header_to_str ~incl ~excl LE.this

let exec_fetch_text (module LE:LazyEmail_inst) =
  LE.LazyEmail.raw_content LE.this

(** build body sectin reply string **)
let body_template_str (prefix:string) (content:string) (sec:sectionPart)
(part:bodyPart) : string =
  let (part,str) = body_part_str part content in
  let sec = section_part_str sec in
  String.concat "" ["BODY[" ; sec ; prefix ; "]" ; part ; crlf ; str]
  
let mk_headers_fetch_str prefix headers =
  String.concat "" [prefix ; " " ; 
  "("; String.concat " " (List.map String.uppercase headers); ")"]

let exec_fetch_msgtext email (msgtext:sectionMsgtext) (sec:sectionPart) (part:bodyPart) =
  (match msgtext with
  | Header -> return ("HEADER", exec_fetch_header email )
  | HeaderFields incl -> 
    return (mk_headers_fetch_str "HEADER.FIELDS" incl,
      exec_fetch_header ~incl:(`Map (map_of_alist incl)) email )
  | HeaderFieldsNot excl -> 
    return (mk_headers_fetch_str "HEADER.FIELDS.NOT" excl,
      exec_fetch_header ~excl:(map_of_alist excl) email )
  | Text -> exec_fetch_text email >>= fun text -> return ("TEXT",text )
  ) >>= fun (prefix,str) ->
  return (body_template_str prefix str sec part)

let exec_fetch_mime email (sec:sectionPart) (part:bodyPart) =
  let incl = `Regx "^content-" in
  let str = exec_fetch_header ~incl email in
  body_template_str "MIME" str sec part

let secpart_to_str secPart =
  String.concat ":" (List.map string_of_int secPart)

let exec_fetch_email_body (module LE:LazyEmail_inst) (sec:sectionPart) (part:bodyPart) =
  begin
  if sec = [] then
    LE.LazyEmail.to_string LE.this
  else
    LE.LazyEmail.raw_content LE.this
  end >>= fun content ->
  return (body_template_str "" content sec part)

let content_type (module LE:LazyEmail_inst) =
  let headers = LE.LazyEmail.header ~incl:(`Regx "^content-type") LE.this in
  let (n,v) = List.hd headers in
  let v = Regex.replace ~regx:"\n" ~tmpl:"" (String.trim v) in
  let v = Regex.replace ~regx:"[ \t]+" ~tmpl:" " v in
  v

let rec walk_debug lazy_email =
  let rec _walk_debug (module LE:LazyEmail_inst) indnt multi =
    let ctype = content_type (module LE) in
    Printf.fprintf stderr "%s%s %s\n%!" indnt multi ctype;
    LE.LazyEmail.content LE.this >>= function
    | `Data _ -> Printf.fprintf stderr "%scontent\n%!" indnt; return ()
    | `Message m -> Printf.fprintf stderr "%smessage\n%!" indnt;
      _walk_debug (lazy_email_of_t (module LE.LazyEmail) m) (indnt ^ "  ") ""
    | `Multipart lemail -> Printf.fprintf stderr "%smultipart:%d\n%!" indnt (List.length lemail);
      Lwt_list.iteri_s (fun i m ->
        _walk_debug (lazy_email_of_t (module LE.LazyEmail) m) (indnt ^ "  ")
        (string_of_int (i+1))
      ) lemail
  in _walk_debug lazy_email "" ""
  
exception SectionDone
(** find the requested section, return empty email if not found **)
let find_fetch_section email secPart =
  let rec walk (module LE:LazyEmail_inst) secPart = 
    catch ( fun () ->
      match secPart with
      | [] -> raise SectionDone
      | hd :: [] ->
        begin
        LE.LazyEmail.content LE.this >>= function
        | `Data _ ->
          if hd = 1 then
            return (module LE:LazyEmail_inst)
          else
            raise SectionDone
        | `Message m ->
          let lazy_email = lazy_email_of_t (module LE.LazyEmail) m in
          let ctype = content_type lazy_email in
          if match_regex ~case:false ~regx:"multipart" ctype then
            walk (lazy_email_of_t (module LE.LazyEmail) m) secPart
          else if hd = 1 then
            return (module LE:LazyEmail_inst)
          else
            raise SectionDone
        | `Multipart lemail ->
          if hd > List.length lemail then
            raise SectionDone
          else
            return (lazy_email_of_t (module LE.LazyEmail) (List.nth lemail (hd - 1)))
        end
      | hd :: tl ->
        LE.LazyEmail.content LE.this >>= function
        | `Data _ -> raise SectionDone
        | `Message m -> 
          walk (lazy_email_of_t (module LE.LazyEmail) m) secPart
        | `Multipart lemail ->
          if hd > List.length lemail then
            raise SectionDone
          else
            walk (lazy_email_of_t (module LE.LazyEmail) (List.nth lemail (hd - 1))) tl

    ) (fun ex -> 
      Printf.fprintf stderr "%s\n%!" (Printexc.to_string ex);
      return (lazy_email_of_t (module LE.LazyEmail) LE.LazyEmail.empty))
  in
  walk email secPart

let exec_fetch_sectext email (secPart:sectionPart) (secText:sectionText option)
(spec:sectionSpec) (part:bodyPart) =
  (*walk_debug email >>*)
  find_fetch_section email secPart >>= fun email -> 
  match secText with 
  | None -> exec_fetch_email_body email secPart part
  | Some secText ->
      match secText with
      | SectionMsgtext msgtext -> exec_fetch_msgtext email msgtext secPart part
      | Mime -> return (exec_fetch_mime email secPart part)

let exec_fetch_body_section (spec:sectionSpec) (part:bodyPart) email =
  match spec with
  | SectionMsgtext msgtext -> 
      (
      match msgtext with 
      |None -> exec_fetch_email_body email [] part
      |Some msgtext -> exec_fetch_msgtext email msgtext [] part
      )
  | SectionPart (secPart,secText) -> exec_fetch_sectext email secPart secText spec part

let get_params_of_list l def =
  let params = 
  try
    List.fold_left (fun acc nv ->
      let nv = String.trim nv in
      if match_regex nv ~regx:"^[ ]*\\([^= ]+\\)[ ]*=[ ]*\\([^ ]+\\|\"[^\"]+\"\\)[ ]*$" then (
        let name = Str.matched_group 1 nv in
        let value = Str.matched_group 2 nv in
        let nv = String.concat "" [quote name ; " " ; quote value] in
        if acc = def then
          nv
        else
          String.concat "" [acc ; " " ; nv]
      ) else ( 
        acc
      )
    ) def l
  with _ -> def 
  in
  params

let get_params_of_str ~str ~def =
  let l = Str.split (Str.regexp "[ ]*;[ ]*") str in
  get_params_of_list l def

(** body type/subtype and parameters
text/plain ; charset=us-ascii
message/rfc822 ; name="Re: thread test.eml"
**)
let fetch_type_and_param content_type_val : (string*string*string) =
  let def_type = quote "text" in
  let def_subtype = quote "plain" in
  let def_params = (quote "charset") ^ (quote "us-ascii") in
  match Str.split (Str.regexp ";") content_type_val with
  | [] -> (def_type, def_subtype, list_of def_params)
  | type_subtype :: params ->
    let type_subtype = String.trim type_subtype in
    let tspecials = "][()><@,;:\\/?=" in
    let token = "a-z0-9_-" in
    let quoted_re = quote (String.concat "" ["[" ; tspecials ; token ; "]+"]) in
    let token_re = String.concat "" ["[" ; token ; "]+"] in
    let value_re = group (orx token_re quoted_re) in
    let attribute_re = group token_re in
    let re = String.concat "" ["^" ; attribute_re ; "/" ; value_re ; "$"] in
    let (t,st) =
    if match_regex ~case:false type_subtype ~regx:re then 
      (Str.matched_group 1 type_subtype,Str.matched_group 2 type_subtype)
    else
      (def_type,def_subtype)
    in
    (quote t,quote st,list_of (get_params_of_list params def_params))

(*
Content-Disposition: inline;
        filename=rose.jpg
Content-Disposition: attachment;
        filename="Re: thread test.eml"
("attachment" ("filename" "Re: thread test.eml")
("inline" ("filename" "rose.jpg"))
*)
let fetch_disposition disp =
    let disp = replace ~regx:"\r" ~tmpl:"" disp in
    let disp = replace ~regx:"\n" ~tmpl:"" disp in
    if match_regex disp ~regx:"^[ \t]*\\([^; \t]+\\)\\([ \t]*;[ \t]*\\(.+\\)\\)$" then
      let name = Str.matched_group 1 disp in
      let params = Str.matched_group 3 disp in
      let params = get_params_of_str ~str:params ~def:"NIL" in
      list_of (String.concat "" [(quote name) ; " " ; (list_of params)])
    else
      list_of (quote disp)

(** some bodystructure fields **)
let fetch_bodystructure_fields (module LE:LazyEmail_inst) : bodystr_fields =
  let headers = LE.LazyEmail.header LE.this in
  let bsize = string_of_int (LE.LazyEmail.size LE.this) in
  let blines = string_of_int (LE.LazyEmail.lines LE.this) in
  List.fold_left (fun acc (n,v) ->
    let v = String.trim v in
    if match_regex ~case:false n ~regx:"content-type" then (
      let (btype,bsubtype,bparams) = fetch_type_and_param v in
      {acc with btype;bsubtype;bparams}
    ) else if match_regex ~case:false n ~regx:"content-id" then (
      {acc with bid = quote v}
    ) else if match_regex ~case:false n ~regx:"content-description" then (
      {acc with bdescr = quote v}
    ) else if match_regex ~case:false n ~regx:"content-transfer-encoding" then (
      {acc with benc = quote v}
    ) else if match_regex ~case:false n ~regx:"content-disposition" then (
      {acc with bdisp = fetch_disposition v}
    ) else
      acc
  ) {default_bodystr_fields with bsize;blines} headers

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
  let all = List.concat [main;extension] in
  String.concat " " all

let fetch_message_bodystructure (module LE:LazyEmail_inst) basic body bodystructure_folder =
  exec_fetch_envelope_unf (LE.LazyEmail.header LE.this) >>= fun env ->
  bodystructure_folder (module LE:LazyEmail_inst) body >>= fun bodystr ->
  let main =String.concat "" [(format_basic basic) ; " " ; (list_of env) ; bodystr ; " " ; basic.blines] in
  (** md5, disposition, language, location **)
  let all =
    if body = false then
      String.concat " " [main ; "NIL" ; basic.bdisp ; "NIL" ; "NIL"]
    else
      main
  in
  return (list_of (all))

let fetch_simple_bodystructure (basic:bodystr_fields) (body:bool) : string =
  list_of (format_simple basic body)

let fetch_multipart_bodystructure lazyemail lemail (basic:bodystr_fields) body bodystructure_folder =
  Lwt_list.fold_left_s
  (fun acc email ->
    let email = lazy_email_of_t lazyemail email in
    bodystructure_folder email body >>= fun bodystr ->
    return (acc ^ bodystr)
  ) "" lemail >>= fun main ->
  (** params, disposition, language, location **)
  let all =
  if body = false then 
    String.concat " " [main; basic.bsubtype; basic.bparams ; basic.bdisp ; "NIL" ; "NIL"]
  else
    String.concat " " [main; basic.bsubtype]
  in
  return (list_of all)

let rec fold_email_bodystructure (module LE:LazyEmail_inst) body =
  let basic = fetch_bodystructure_fields (module LE:LazyEmail_inst) in
  LE.LazyEmail.content LE.this >>= fun cont ->
  match cont with
  | `Data _ -> return (fetch_simple_bodystructure basic body)
  | `Message email -> 
    let email = lazy_email_of_t (module LE.LazyEmail) email in
    fetch_message_bodystructure email basic body fold_email_bodystructure
  | `Multipart lemail -> 
    fetch_multipart_bodystructure (module LE.LazyEmail) lemail basic body fold_email_bodystructure

(** bodyscture for the whole email-message **)
let exec_fetch_bodystructure email =
  fold_email_bodystructure email false >>= fun str ->
  return ("BODYSTRUCTURE " ^ str)

(** bodyscture for the whole email-message **)
let exec_fetch_body email =
  fold_email_bodystructure email true >>= fun str ->
  return ("BODY " ^ str)

let exec_fetch_att seq sequence message att =
  let email (module LM:LazyMessage_inst) f =
    LM.LazyMessage.get_email LM.this >>= fun email ->
    f email
  in
  let headers message f =
    get_header message >>= fun headers ->
    f headers
  in
  let metadata (module LM:LazyMessage_inst) f =
    LM.LazyMessage.get_message_metadata LM.this >>= fun meta ->
    f meta
  in
  Lwt_list.map_s
  (fun item ->
    match item with
    | Fetch_Body -> email message exec_fetch_body 
    (** TBD set \Seen **)
    | Fetch_BodySection (spec,part) -> email message (exec_fetch_body_section spec part)
    (** TBD no setting \Seen **)
    | Fetch_BodyPeekSection (spec,part) -> email message (exec_fetch_body_section spec part)
    | Fetch_Bodystructure -> email message exec_fetch_bodystructure 
    | Fetch_Envelope -> headers message exec_fetch_envelope 
    | Fetch_Flags -> metadata message exec_fetch_flags 
    | Fetch_Modseq -> metadata message exec_fetch_modseq 
    | Fetch_Internaldate -> metadata message exec_fetch_internaldate 
    | Fetch_Rfc822 -> email message exec_fetch_rfc822 
    | Fetch_Rfc822Header -> email message exec_fetch_rfc822header 
    | Fetch_Rfc822Size -> email message exec_fetch_rfc822size 
    | Fetch_Rfc822Text -> email message exec_fetch_rfc822text 
    | Fetch_Uid -> metadata message exec_fetch_uid
  ) att >>= fun l ->
  return (String.concat " " l)

let exec_fetch_macro seq sequence message macro =
  match macro with
  | Fetch_All -> exec_fetch_att seq sequence message
    [Fetch_Flags;Fetch_Internaldate;Fetch_Rfc822Size;Fetch_Envelope]
  | Fetch_Fast -> exec_fetch_att seq sequence message
    [Fetch_Flags;Fetch_Internaldate;Fetch_Rfc822Size]
  | Fetch_Full -> exec_fetch_att seq sequence message
    [Fetch_Flags;Fetch_Internaldate;Fetch_Rfc822Size;Fetch_Envelope;Fetch_Body]
      

let exec_fetch_all seq sequence message fetchattr buid =
  match fetchattr with
  | FetchMacro macro -> exec_fetch_macro seq sequence message macro
  | FetchAtt att -> exec_fetch_att seq sequence message att

(* is it possible to avoid reading the metadata all the time? TBD *)
let exec_fetch seq sequence (module LM:LazyMessage_inst) attr changedsince buid =
  LM.LazyMessage.get_message_metadata LM.this >>= fun record ->
  if (buid = false && exec_seq sequence seq || buid = true && (exec_seq sequence record.uid))  &&
    (changedsince = None || Int64.compare record.modseq (option_value_exn
    changedsince) > 0) then (
      exec_fetch_all seq sequence (module LM:LazyMessage_inst) attr buid >>= fun str ->
      let seq = string_of_int seq in
      return (Some (String.concat space [seq ; "FETCH" ; list_of str]))
  ) else
    return None

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
      String.concat "" ["UID " ; string_of_int record.uid ; space]
    else
      ""
  in
  let modseq = 
    if modseq then
      String.concat "" ["MODSEQ (" ; (Int64.to_string record.modseq) ; ") "] 
    else
      ""
  in
  String.concat "" [seq ; space ; "FETCH (" ; uid ; modseq ; "FLAGS (" ; flags ; ")"]

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
