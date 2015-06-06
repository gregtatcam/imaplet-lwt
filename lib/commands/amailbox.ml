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
open Storage_meta
open Imaplet_types
open Irmin_storage

exception InvalidStorageType

type selection = [`Select of string | `Examine of string | `None]

type amailboxt = 
  {inbox_path:string;mail_path:string;user:string
  option;selected:selection;config:Server_config.imapConfig;
  keys:(Ssl_.keys Lwt.t) option}

let factory (mailboxt:amailboxt) ?mailbox2 mailbox =
  let open Server_config in
  let open Storage in
  let open Mailbox_storage in
  let open Maildir_storage in
  Utils.option_value_exn mailboxt.keys >>= fun keys ->
  match mailboxt.config.data_store with
  | `Irmin -> 
    build_strg_inst (module IrminStorage) mailboxt.config (Utils.option_value_exn mailboxt.user)
    ?mailbox2 mailbox keys
  | `GitWorkdir -> 
    build_strg_inst (module GitWorkdirStorage) mailboxt.config (Utils.option_value_exn mailboxt.user)
    ?mailbox2 mailbox keys
  | `Mailbox ->
    build_strg_inst (module MailboxStorage) mailboxt.config (Utils.option_value_exn mailboxt.user)
    ?mailbox2 mailbox keys
  | `Maildir ->
    build_strg_inst (module MaildirStorage) mailboxt.config (Utils.option_value_exn mailboxt.user)
    ?mailbox2 mailbox keys

(* inbox location * all mailboxes location * user * type of selected mailbox *)
type t = amailboxt

let selected_mbox mailboxt =
  match mailboxt.selected with
  | `None -> None
  | `Select s -> Some s
  | `Examine s -> Some s

(* create the mailbox type *)
let create config user pswd =
  let open Server_config in
  let (inbox_path,mail_path) = 
  match config.data_store with
  | `Irmin|`GitWorkdir -> "",""
  | `Mailbox|`Maildir -> config.inbox_path,Configuration.mailboxes config.mail_path user
  in
  {inbox_path;mail_path;user=Some user;selected=`None;config; keys=Some (Ssl_.get_user_keys ~user ?pswd config)}

(* empty type *)
let empty () =
  {inbox_path="";mail_path="";user=None;selected=`None;config=Server_config.default_config;keys=None}

(* get authenticated user *)
let user mailboxt =
  mailboxt.user

(** make directory item **)
let dir_item path cnt =
  (path, "\\Noselect" ::
    if cnt = 0 then
      ["\\HasNoChildren"]
    else
      ["\\HasChildren"])

(** make mailbox item **)
let mbox_item path mailbox cnt =
  let flags =
  if cnt = 0 then
    "\\HasNoChildren"
  else
    "\\HasChildren" 
  in
  (path, flags ::
  if mailbox = "" then 
  (
    if path = "Drafts" then
      ["\\Drafts"]
    else if path = "Deleted Messages" then
      ["\\Trash"]
    else if path = "Sent Messages" then
      ["\\Sent"] 
    else
      []
  ) else
    []
  )
  
let list_ mailboxt subscribed mailbox wilcards =
  let open Regex in
  let open Utils in
  (* item is relative to the mailbox *)
  let select_item acc path item cnt =
    (** get item path relative to the relative root **)
    if match_regex path ~regx:wilcards then
      ((item cnt) :: acc)
    else
      acc
  in
  (* item has path relative to the start, i.e. root + mailbox *)
  factory mailboxt mailbox >>= fun (module Mailbox) ->
  Mailbox.MailboxStorage.list Mailbox.this ~subscribed ~access:(fun _ -> true) 
  ~init:[] ~f:(fun acc item ->
    let path item = concat_path mailbox item in
    match item with
      | `Folder (item,cnt)  -> return (select_item acc (path item) (dir_item (path item)) cnt)
      | `Mailbox (item,cnt)  -> return (select_item acc (path item) (mbox_item (path item) mailbox) cnt)
  )

(** add to the calculated list the reference folder and inbox **)
let add_list reference mailbox acc =
  let open Regex in
  let fixed = fixregx_mbox mailbox in
  if dequote reference = "" && match_regex "INBOX" ~regx:fixed then (
    if List.exists (fun (m,_) -> (String.lowercase m) = "inbox") acc then
      acc
    else
      ("INBOX", ["\\HasNoChildren"])::acc
  ) else
    acc

let get_path_and_regex reference mailbox =
  let open Regex in
  (* match the wild cards part of the mailbox *)
  let fixref = dequote reference in 
  let fixref = replace ~regx:"^/$" ~tmpl:"" fixref in
  let fixmbx = replace ~regx:"\"" ~tmpl:"" mailbox in
  let str = fixref ^ fixmbx in
  if match_regex str ~regx:"\\([^/]*[%\\*].*$\\)" then (
    let regx = Str.matched_string str in
    let len = String.length str - (String.length regx) in
    let path =
      if len = 0 then
        ""
      else
        String.sub str 0 len
    in
    path,(fixregx_mbox regx)
  ) else (
    str,""
  )

(** list mailbox **)
let list_adjusted mailboxt subscribed reference mailbox =
  let open Regex in
  let flags = ["\\Noselect"] in
  if mailbox = "" then (
    (* reference doesn't start with / *)
    if match_regex reference ~regx:"^\"?\\([^/]+/\\)" then
      return ([Str.matched_group 1 reference,flags])
    else 
      return (["/",flags])
  (* reference starts with / *)
  ) else if match_regex reference ~regx:"^\"?/" then (
    return ([])
  ) else (
    let (path,regx) = get_path_and_regex reference mailbox in
    list_ mailboxt subscribed path regx (*>>= 
      fun acc -> return (add_list reference mailbox acc)*)
  )

(** list mailbox **)
let list mailboxt reference mailbox =
  list_adjusted mailboxt false reference mailbox

(** lsubmbx mailbx reference mailbox, list on subscribed mailboxes 
 * need to handle a wild card % case when foo/bar is subscribed but foo is no
 * should return foo only in this case 
**)
let lsub mailboxt reference mailbox =
  list_adjusted mailboxt true reference mailbox 

let select_ mailboxt mailbox selection = 
  factory mailboxt mailbox >>= fun (module Mailbox) ->
  Mailbox.MailboxStorage.exists Mailbox.this >>= function
  | `No -> return `NotExists
  | `Folder -> return `NotSelectable
  | `Mailbox ->
    Mailbox.MailboxStorage.select Mailbox.this >>= fun mailbox_metadata ->
    return (`Ok ({mailboxt with selected = selection}, mailbox_metadata))

(* select mailbox *)
let select mailboxt mailbox = 
  select_ mailboxt mailbox (`Select mailbox)

(* examine mailbox *)
let examine mailboxt mailbox =
  select_ mailboxt mailbox (`Examine mailbox)

(* create mailbox *)
let create_mailbox mailboxt mailbox =
  factory mailboxt mailbox >>= fun (module Mailbox) ->
  Mailbox.MailboxStorage.exists Mailbox.this >>= function
  | `Mailbox -> return (`Error("Mailbox already exists"))
  | `Folder -> return (`Error("Invalid Superior"))
  | `No ->
    let open Regex in
    if match_regex mailbox ~regx:"^/" then
      return (`Error("Invalid mailbox name: Begins with hierarchy separator"))
    else if match_regex mailbox ~regx:"^\"?.imaplet/?\"?" then
      return (`Error("Invalid mailbox name: Contains reserved name"))
    else if match_regex mailbox ~regx:"^\"?./?\"?$" || match_regex mailbox ~regx:"^\"?../?\"?$" then
      return (`Error("Invalid mailbox name: Contains . part"))
    else 
    (
      Mailbox.MailboxStorage.create_mailbox Mailbox.this >> 
      Mailbox.MailboxStorage.commit Mailbox.this >>
      return `Ok
    )

(* delete mailbox *)
let delete_mailbox mailboxt mailbox =
  factory mailboxt mailbox >>= fun (module Mailbox) ->
  Mailbox.MailboxStorage.exists Mailbox.this >>= function
  | `No -> return (`Error("Mailbox doesn't exist"))
  | _ ->
    Mailbox.MailboxStorage.delete Mailbox.this >>
    Mailbox.MailboxStorage.commit Mailbox.this >>
    return `Ok

(* rename mailbox *)
let rename_mailbox mailboxt src dest =
  factory mailboxt ?mailbox2:(Some dest) src >>= fun (module Mailbox) ->
  Mailbox.MailboxStorage.exists Mailbox.this >>= function
  | `No -> return (`Error("Mailbox doesn't exist"))
  | _ ->
    Mailbox.MailboxStorage.exists (Utils.option_value_exn Mailbox.this2) >>= function
    | `No ->
      Mailbox.MailboxStorage.rename Mailbox.this dest >>
      Mailbox.MailboxStorage.commit Mailbox.this >>
      Mailbox.MailboxStorage.commit (Utils.option_value_exn Mailbox.this2) >>
      return `Ok
    | _ -> return (`Error ("Destination mailbox exists"))

(* subscribe mailbox *)
let subscribe mailboxt mailbox =
  factory mailboxt mailbox >>= fun (module Mailbox) ->
  Mailbox.MailboxStorage.exists Mailbox.this >>= function
  | `No -> return (`Error("Mailbox doesn't exist"))
  | _ ->
    Mailbox.MailboxStorage.subscribe Mailbox.this >>
    Mailbox.MailboxStorage.commit Mailbox.this >>
    return `Ok

(* unsubscribe mailbox *)
let unsubscribe mailboxt mailbox =
  factory mailboxt mailbox >>= fun (module Mailbox) ->
  Mailbox.MailboxStorage.exists Mailbox.this >>= function
  | `No -> return (`Error("Mailbox doesn't exist"))
  | _ ->
    Mailbox.MailboxStorage.unsubscribe Mailbox.this >>
    Mailbox.MailboxStorage.commit Mailbox.this >>
    return `Ok

let get_message_from_client reader writer literal =
  (** request the message from the client **)
  begin
    match literal with
    | Literal n -> Response.write_resp Int64.zero writer (Resp_Cont("")) >> return n
    | LiteralPlus n -> return n
  end >>= fun size ->
  let message = String.create size in
  (* timeout and how long? what if the message fairly big? *)
  Lwt_io.read_into_exactly reader message 0 size >>= fun () ->
  let message = Utils.make_email_message message in
  return (message,size)

let find_fl flags fl =
  Utils.list_find flags (fun f -> f = fl)

(* append message to mailbox *)
let append mailboxt mailbox reader writer flags date literal =
  factory mailboxt mailbox >>= fun (module Mailbox) ->
  Mailbox.MailboxStorage.exists Mailbox.this >>= function
    | `No -> return (`NotExists)
    | `Folder -> return (`NotSelectable)
    | `Mailbox -> 
      get_message_from_client reader writer literal >>= fun (message,size) ->
      let flags = 
      match flags with
      | None -> [Flags_Recent]
      | Some flags -> (Flags_Recent :: flags)
      in
      Mailbox.MailboxStorage.status Mailbox.this >>= fun mailbox_metadata ->
      let modseq = Int64.add mailbox_metadata.modseq Int64.one in
      let message_metadata = {
        uid = mailbox_metadata.uidnext;
        modseq;
        size;
        internal_date = Utils.option_value date ~default:(Dates.ImapTime.now());
        flags;
      } in
      let seen = find_fl flags Flags_Seen in
      let mailbox_metadata = { mailbox_metadata with
        uidnext = mailbox_metadata.uidnext + 1;
        count = mailbox_metadata.count + 1;
        recent = mailbox_metadata.recent + 1;
        unseen = 
          if seen = false && mailbox_metadata.unseen = 0 then
            mailbox_metadata.count + 1  
          else
            mailbox_metadata.unseen
        ;
        nunseen = 
          if seen = false then
            mailbox_metadata.nunseen + 1
          else
            mailbox_metadata.unseen
        ;
        modseq
      } in
      Mailbox.MailboxStorage.store_mailbox_metadata Mailbox.this mailbox_metadata >>
      Mailbox.MailboxStorage.append Mailbox.this message message_metadata >>
      Mailbox.MailboxStorage.commit Mailbox.this >>
      return `Ok

(* close selected mailbox *)
let close mailboxt =
  {mailboxt with selected=`None}

(* iterator for message number or uid sequence *)
let get_iterator (module Mailbox: Storage.Storage_inst) buid sequence rev =
  let open Seq_iterator in
  let single = SequenceIterator.single sequence in
  if single <> None then (
    return (Some (SequenceIterator.create sequence ~rev
      (Utils.option_value_exn single) (Utils.option_value_exn single) ))
  ) else (
    Mailbox.MailboxStorage.status Mailbox.this >>= fun mailbox_metadata ->
    if buid = false then (
      return (Some (SequenceIterator.create sequence ~rev 1 mailbox_metadata.count))
    ) else (
      Mailbox.MailboxStorage.fetch_message_metadata Mailbox.this (`Sequence 1) >>= function
      | `Ok message_metadata ->
        return (Some (SequenceIterator.create sequence ~rev message_metadata.uid
          (mailbox_metadata.uidnext - 1)))
      | _ -> return None
    )
  )

(* iterate over the sequence, call f for each position *)
let iter_selected_with_seq (module Mailbox:Storage.Storage_inst) sequence ?(rev=false) buid f acc =
  let open Interpreter in
  get_iterator (module Mailbox) buid sequence rev >>= function
  | None -> return acc
  | Some it ->
    let open Seq_iterator in
    let rec read acc = function
      | `End -> return acc
      | `Ok seq ->
        let pos = (if buid then (`UID seq) else (`Sequence seq)) in
        begin 
          if buid then Mailbox.MailboxStorage.uid_to_seq Mailbox.this seq else return (Some seq) 
        end >>= function
        | None -> read acc (SequenceIterator.next it)
        | Some seq ->
          f acc pos seq >>= function
          | `Eof acc -> return acc
          | `Ok acc -> read acc (SequenceIterator.next it)
    in
    read acc (SequenceIterator.next it) >>= fun acc ->
    return acc

(* get storage for selected mailbox *)
let get_selected_mailbox ?mailbox2 mailboxt =
  match (selected_mbox mailboxt) with
  | None -> return (`Error "Not selected")
  | Some mailbox ->
  factory mailboxt ?mailbox2 mailbox >>= fun (module Mailbox) ->
  Mailbox.MailboxStorage.exists Mailbox.this >>= function
  | `No -> return (`NotExists)
  | `Folder -> return (`NotSelectable)
  | `Mailbox -> return (`Ok (mailbox,(module Mailbox:Storage.Storage_inst)))

(* search mailbox *)
let search mailboxt resp_prefix keys buid =
  get_selected_mailbox mailboxt >>= function 
  | `NotExists -> return `NotExists
  | `NotSelectable -> return `NotSelectable
  | `Error e -> return (`Error e)
  | `Ok (_,(module Mailbox)) ->
    resp_prefix () >>
    let sequence = [SeqRange (Number 1,Wild)] in
    iter_selected_with_seq (module Mailbox) sequence buid (fun (modseq,acc) pos seq ->
      Mailbox.MailboxStorage.fetch Mailbox.this pos >>= function
      | `Eof -> return (`Eof (modseq,acc))
      | `NotFound -> return (`Ok (modseq,acc))
      | `Ok message -> 
        Interpreter.exec_search message keys seq >>= fun res ->
        if res then (
          let (module LazyMessage) = message in
          LazyMessage.LazyMessage.get_message_metadata LazyMessage.this >>= fun metadata ->
          let modseq = if (Int64.compare metadata.modseq modseq) > 0 then metadata.modseq else modseq in
          return (`Ok (modseq,(if buid then metadata.uid else seq) :: acc))
        ) else
          return (`Ok (modseq,acc))
    ) (Int64.zero,[]) >>= fun (modseq,acc) -> 
      if Interpreter.has_key keys (function
        | Search_Modseq _ -> true
        | _ -> false) then
        return (`Ok (Some modseq, acc))
      else
        return (`Ok (None, acc))

let update_fetch_attr fetchattr changedsince buid =
  match fetchattr with
    | FetchMacro macro -> fetchattr
    | FetchAtt att -> 
      let (is_uid,is_modseq) = List.fold_right (fun i (is_uid,is_modseq) -> 
        if i = Fetch_Uid then (
          true, is_modseq
        ) else if i = Fetch_Modseq then (
          is_uid, true
        ) else
          is_uid, is_modseq
      ) att (false,false) in
      let att = if changedsince <> None && is_modseq = false then Fetch_Modseq :: att else att in
      let att = if buid && is_uid = false then Fetch_Uid :: att else att in
      FetchAtt att

(* fetch data from mailbox *)
let fetch mailboxt resp_prefix resp_writer sequence fetchattr changedsince buid =
  get_selected_mailbox mailboxt >>= function 
  | `NotExists -> return `NotExists
  | `NotSelectable -> return `NotSelectable
  | `Error e -> return (`Error e)
  | `Ok (_,(module Mailbox)) ->
    let fetchattr = update_fetch_attr fetchattr changedsince buid in
    resp_prefix () >>
    iter_selected_with_seq (module Mailbox) sequence buid (fun acc pos seq ->
      Mailbox.MailboxStorage.fetch Mailbox.this pos >>= function
      | `Eof -> return (`Eof acc)
      | `NotFound -> return (`Ok acc)
      | `Ok message -> 
        (* need more efficient exec_fetch since sequence is already tested
         by the iterator *)
        Interpreter.exec_fetch seq sequence message fetchattr changedsince buid >>= fun res ->
        match res with
        | Some res -> resp_writer res; return (`Ok acc)
        | None -> return (`Ok acc)
    ) () >>= fun _ -> return `Ok

(* get mailbox_metadata flag status 
 * based on the original and updated flag value
 *)
let get_update_flag orig_message_metadata upd_message_metadata flag =
  let orig = find_fl orig_message_metadata.flags flag in
  let upd = find_fl upd_message_metadata.flags flag in
  if orig = upd then
    0
  else if orig && upd = false then
    -1
  else
    1

(* update seen,unseen,recent mailbox metadata flags *)
let update_mailbox_metadata_flags seq mailbox_metadata orig_message_metadata
    upd_message_metadata =
  let seen = get_update_flag orig_message_metadata upd_message_metadata Flags_Seen in
  let recent = get_update_flag orig_message_metadata upd_message_metadata Flags_Recent in
  {mailbox_metadata with
    recent = mailbox_metadata.recent + recent;
    nunseen = mailbox_metadata.nunseen - seen;
    unseen = 
      if mailbox_metadata.unseen = 0 && seen = -1 then
        seq
      else if mailbox_metadata.unseen <> 0 && 
            mailbox_metadata.unseen = seq && seen = 1 then
        0
      else
        mailbox_metadata.unseen
    ;
  }

(* store flags *)
let store mailboxt resp_prefix resp_writer sequence storeattr flagsval unchangedsince buid =
  get_selected_mailbox mailboxt >>= function 
  | `NotExists -> return `NotExists
  | `NotSelectable -> return `NotSelectable
  | `Error e -> return (`Error e)
  | `Ok (_,(module Mailbox)) ->
  resp_prefix () >>
  Mailbox.MailboxStorage.status Mailbox.this >>= fun mailbox_metadata ->
  iter_selected_with_seq (module Mailbox) sequence buid (fun (modified,mailbox_metadata) pos seq ->
    Mailbox.MailboxStorage.fetch_message_metadata Mailbox.this pos >>= function
    | `Eof -> return (`Eof (modified,mailbox_metadata))
    | `NotFound -> return (`Ok (modified,mailbox_metadata))
    | `Ok message_metadata ->
      let update_metadata mailbox_metadata orig_message_metadata upd_message_metadata =
        let mailbox_metadata = update_mailbox_metadata_flags seq mailbox_metadata
            orig_message_metadata upd_message_metadata
        in
        let modseq = Int64.add mailbox_metadata.modseq Int64.one in
        let mailbox_metadata = {
          mailbox_metadata with modseq
        } in
        let message_metadata = {upd_message_metadata with modseq} in
        Mailbox.MailboxStorage.store Mailbox.this pos message_metadata >>
        return mailbox_metadata
      in
      match Interpreter.exec_store message_metadata seq sequence storeattr flagsval unchangedsince buid with
      | `Modseqfailed pos -> return (`Ok ((string_of_int pos) :: modified,mailbox_metadata))
      | `None -> return (`Ok (modified,mailbox_metadata))
      | `Silent metadata -> 
        update_metadata mailbox_metadata message_metadata metadata >>= 
        fun mailbox_metadata -> return (`Ok (modified,mailbox_metadata))
      | `Ok (metadata,res) -> 
        resp_writer res >>
        update_metadata mailbox_metadata message_metadata metadata >>=
        fun mailbox_metadata -> return (`Ok (modified,mailbox_metadata))
  ) ([],mailbox_metadata) >>= fun (modified,mailbox_metadata) ->
  Mailbox.MailboxStorage.store_mailbox_metadata Mailbox.this mailbox_metadata >>
  Mailbox.MailboxStorage.commit Mailbox.this >>
  return (`Ok modified)

(* copy messages to mailbox *)
let copy mailboxt dest_mbox sequence buid =
  get_selected_mailbox ?mailbox2:(Some dest_mbox) mailboxt >>= function 
  | `NotExists -> return `NotExists
  | `NotSelectable -> return `NotSelectable
  | `Error e -> return (`Error e)
  | `Ok (_,(module Mailbox)) ->
  let mbox2 = Utils.option_value_exn Mailbox.this2 in
  Mailbox.MailboxStorage.exists mbox2 >>= function
  | `No -> return (`NotExists)
  | `Folder -> return (`NotSelectable)
  | `Mailbox -> 
  Mailbox.MailboxStorage.status mbox2 >>= fun mailbox_metadata2 ->
  iter_selected_with_seq (module Mailbox) sequence buid (fun mailbox_metadata2 pos seq ->
    (* fetch message from the source mailbox *)
    Mailbox.MailboxStorage.fetch_message_metadata Mailbox.this pos >>= function
    | `Eof -> return (`Eof mailbox_metadata2)
    | `NotFound -> return (`Ok mailbox_metadata2)
    | `Ok message_metadata ->
      let uidnext = mailbox_metadata2.uidnext in
      let modseq = Int64.add mailbox_metadata2.modseq Int64.one in
      let message_metadata = {message_metadata with 
        uid = uidnext;
        flags =
        if find_fl message_metadata.flags Flags_Recent then
          message_metadata.flags
        else
          Flags_Recent :: message_metadata.flags
        ;
        modseq
      }
      in
      let seen = find_fl message_metadata.flags Flags_Seen in
      let mailbox_metadata2 = {mailbox_metadata2 with
        uidnext = uidnext + 1;
        count = mailbox_metadata2.count + 1;
        recent = mailbox_metadata2.recent + 1;
        nunseen = 
          if seen then
            mailbox_metadata2.nunseen
          else
            mailbox_metadata2.nunseen + 1
        ;
        unseen =
          if mailbox_metadata2.unseen <> 0 then
            mailbox_metadata2.unseen
          else
            mailbox_metadata2.count + 1
        ;
        modseq
      }
      in
      (* copy message to the destination mailbox *)
      Mailbox.MailboxStorage.copy Mailbox.this pos mbox2 message_metadata >>
      return (`Ok mailbox_metadata2)
  ) mailbox_metadata2 >>= fun mailbox_metadata2 ->
  Mailbox.MailboxStorage.store_mailbox_metadata mbox2 mailbox_metadata2 >>
  Mailbox.MailboxStorage.commit mbox2 >>
  return `Ok

(* permanently remove messages with \Deleted flag *)
let expunge mailboxt resp_writer =
  let sequence = [SeqRange (Number 1, Wild)] in
  get_selected_mailbox mailboxt >>= function 
  | `NotExists -> return `NotExists
  | `NotSelectable -> return `NotSelectable
  | `Error e -> return (`Error e)
  | `Ok (_,(module Mailbox)) ->
  Mailbox.MailboxStorage.status Mailbox.this >>= fun mailbox_metadata ->
  iter_selected_with_seq (module Mailbox) sequence ~rev:true true (fun mailbox_metadata pos seq ->
    Mailbox.MailboxStorage.fetch_message_metadata Mailbox.this pos >>= function
    | `Eof -> return (`Eof mailbox_metadata)
    | `NotFound -> return (`Ok mailbox_metadata)
    | `Ok message_metadata ->
    let deleted = find_fl message_metadata.flags Flags_Deleted in
    if deleted = false then
      let seen = find_fl message_metadata.flags Flags_Seen in
      let recent = find_fl message_metadata.flags Flags_Recent in
      let mailbox_metadata = {mailbox_metadata with
        count = mailbox_metadata.count + 1;
        recent = 
          if recent then
            mailbox_metadata.recent + 1
          else
            mailbox_metadata.recent
        ;
        nunseen =
          if seen = false then
            mailbox_metadata.nunseen + 1
          else
            mailbox_metadata.nunseen
        ;
        unseen =
          if seen = false && mailbox_metadata.unseen = 0 then
            mailbox_metadata.count + 1
          else
            mailbox_metadata.unseen
        ;
      } in
      return (`Ok mailbox_metadata)
    else (
      Mailbox.MailboxStorage.delete_message Mailbox.this pos >>
      resp_writer (string_of_int seq) >>
      return (`Ok mailbox_metadata)
    )
  ) {mailbox_metadata with count=0;recent=0;nunseen=0;unseen=0} >>= fun mailbox_metadata ->
    Mailbox.MailboxStorage.store_mailbox_metadata Mailbox.this mailbox_metadata >>
    Mailbox.MailboxStorage.commit Mailbox.this >>
    return `Ok
