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

type amailboxt = {inbox_path:string;mail_path:string;user:string option;selected:selection}

let factory mailboxt ?mailbox2 mailbox =
  let open Server_config in
  let open Storage in
  let open Mailbox_storage in
  let open Core.Std in
  match srv_config.data_store with
  | `Irmin -> 
    build_strg_inst (module IrminStorage) (Option.value_exn mailboxt.user)
    ?mailbox2 mailbox
  | `Mailbox ->
    build_strg_inst (module MailboxStorage) (Option.value_exn mailboxt.user)
    ?mailbox2 mailbox

(* inbox location * all mailboxes location * user * type of selected mailbox *)
type t = amailboxt

let selected_mbox mailboxt =
  match mailboxt.selected with
  | `None -> None
  | `Select s -> Some s
  | `Examine s -> Some s

(* create the mailbox type *)
let create user =
  let open Server_config in
  let (inbox_path,mail_path) = 
  match srv_config.data_store with
  | `Irmin -> "",""
  | `Mailbox -> srv_config.inbox_path,srv_config.mail_path
  in
  {inbox_path;mail_path;user=Some user;selected=`None}

(* empty type *)
let empty () =
  {inbox_path="";mail_path="";user=None;selected=`None}

(* get authenticated user *)
let user mailboxt =
  mailboxt.user

(** make directory item **)
let dir_item item cnt =
  (item, "\\Noselect" ::
    if cnt = 0 then
      ["\\HasNoChildren"]
    else
      ["\\HasChildren"])

(** make mailbox item **)
let mbox_item path mailbox =
  (path, "NoInferiors" ::
  if mailbox = "" then 
  (
    if path = "Drafts" then
      ["\\Drafts"]
    else if path = "Deleted Messages" then
      ["\\Deleted"]
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
  let open Core.Std in
  (* item is relative to the mailbox *)
  let select_item acc mailbox item isdir =
    (** get item path relative to the relative root **)
    let path = concat_path mailbox item in
    Printf.printf "list matching %s %s\n%!" mailbox wilcards;
    if isdir <> None then
    (
      if match_regex path ~regx:wilcards then
        ((dir_item path (Option.value_exn isdir)) :: acc)
      else
        acc
    ) else if match_regex path ~regx:wilcards then
        ((mbox_item path mailbox) :: acc)
    else
      (acc)
  in
  (* item has path relative to the start, i.e. root + mailbox *)
  factory mailboxt mailbox >>= fun (module Mailbox) ->
  Mailbox.MailboxStorage.list Mailbox.this ~subscribed ~access:(fun _ -> true) 
  ~init:[] ~f:(fun acc item ->
    match item with
      | `Folder (item,cnt)  -> return (select_item acc mailbox item (Some cnt))
      | `Mailbox item  -> return (select_item acc mailbox item None)
  )

(** add to the calculated list the reference folder and inbox **)
let add_list reference mailbox acc =
  let open Regex in
  let fixed = fixregx_mbox mailbox in
  if dequote reference = "" && match_regex "INBOX" ~regx:fixed then 
    ("INBOX", ["\\HasNoChildren"])::acc
  else
    acc

let get_path_and_regex reference mailbox =
  let open Regex in
  let open Core.Std in
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
        String.slice str 0 len
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
    Printf.printf "special listmbx -%s- -%s-\n%!" reference mailbox;
    (* reference doesn't start with / *)
    if match_regex reference ~regx:"^\"?\\([^/]+/\\)" then
      return ([Str.matched_group 1 reference,flags])
    else 
      return (["/",flags])
  (* reference starts with / *)
  ) else if match_regex reference ~regx:"^\"?/" then (
    Printf.printf "special blank listmbx -%s- -%s-\n%!" reference mailbox;
    return ([])
  ) else (
    let (path,regx) = get_path_and_regex reference mailbox in
    Printf.printf "regular listmbx -%s- -%s- -%s- -%s-\n%!" reference mailbox path regx;
    list_ mailboxt subscribed path regx >>= 
      fun acc -> return (add_list reference mailbox acc)
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
  let open Core.Std in
  factory mailboxt ?mailbox2:(Some dest) src >>= fun (module Mailbox) ->
  Mailbox.MailboxStorage.exists Mailbox.this >>= function
  | `No -> return (`Error("Mailbox doesn't exist"))
  | _ ->
    Mailbox.MailboxStorage.exists (Option.value_exn Mailbox.this2) >>= function
    | `No ->
      Mailbox.MailboxStorage.rename Mailbox.this dest >>
      Mailbox.MailboxStorage.commit Mailbox.this >>
      Mailbox.MailboxStorage.commit (Option.value_exn Mailbox.this2) >>
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

(* append message to mailbox *)
let append mailboxt mailbox reader writer flags date literal =
  let open Core.Std in
  factory mailboxt mailbox >>= fun (module Mailbox) ->
  Mailbox.MailboxStorage.exists Mailbox.this >>= function
    | `No -> return (`NotExists)
    | `Folder -> return (`NotSelectable)
    | `Mailbox -> 
      (** request the message from the client **)
      begin
        match literal with
        | Literal n -> Response.write_resp writer (Resp_Cont("")) >> return n
        | LiteralPlus n -> return n
      end >>= fun size ->
      let message = String.create size in
      (* timeout and how long? what if the message fairly big? *)
      Lwt_io.read_into_exactly reader message 0 size >>= fun () ->
      let headers = String.slice message 0 (if size < 1024 * 5 then size else 1024 * 5) in
      let message =
      if Regex.match_regex headers ~regx:"^From [^\r\n]+" then ( 
        let post = Str.matched_group 0 headers in
        let email = Str.last_chars message (size - (String.length post)) in
        {Email_message.Mailbox.Message.postmark=Email_message.Mailbox.Postmark.of_string post; 
          Email_message.Mailbox.Message.email = Email_message.Email.of_string email}
      ) else (
        (* try to construct the postmark, since the message could be malformed,
        * look at a slice that should include the headers
        *)
        let time = Time.to_float (Time.now()) in
        let tm = Unix.gmtime time in
        let date_time () = Printf.sprintf "%s %s %d %02d:%02d:%02d %d"
          (Dates.day_of_week tm.Unix.tm_wday) (Dates.int_to_month
          tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
          tm.Unix.tm_sec (1900+tm.Unix.tm_year)
        in
        let from buff =
          if Regex.match_regex headers ~regx:"^From: \\([^<]+\\)<\\([^>]+\\)" then
            Str.matched_group 2 buff
          else
            "From daemon@localhost.local"
        in
        let post = ("From " ^ (from headers) ^ " " ^ (date_time ()) ^ "\r\n") in
        {Email_message.Mailbox.Message.postmark=Email_message.Mailbox.Postmark.of_string post; 
          Email_message.Mailbox.Message.email = Email_message.Email.of_string message}
      )
      in 
      let flags = Option.value flags ~default:[] in
      let flags =
        if List.find flags ~f:(fun f -> if f = Flags_Recent then true else false) <> None then
         Some flags
        else
          Some (Flags_Recent :: flags)
      in
      let dt = Some (Option.value date ~default:Time.epoch) in
      let metadata = empty_mailbox_message_metadata() in
      let metadata = update_mailbox_message_metadata ~data:metadata
            ?internal_date:dt ?flags ()
      in
      Mailbox.MailboxStorage.append Mailbox.this message metadata >>
      Mailbox.MailboxStorage.commit Mailbox.this >>
      return `Ok

(* close selected mailbox *)
let close mailboxt =
  {mailboxt with selected=`None}

(* search mailbox *)
let search mailboxt keys buid =
  match (selected_mbox mailboxt) with
  | None -> return (`Error "Not selected")
  | Some name ->
  factory mailboxt name >>= fun (module Mailbox) ->
  Mailbox.MailboxStorage.exists Mailbox.this >>= function
  | `No -> return (`NotExists)
  | `Folder -> return (`NotSelectable)
  | `Mailbox -> 
    Mailbox.MailboxStorage.search Mailbox.this keys buid >>= fun acc -> 
    return (`Ok acc)

let get_iterator (module Mailbox: Storage.Storage_inst) buid sequence =
  let open Seq_iterator in
  let open Core.Std in
  if SequenceIterator.single sequence then (
    return (Some (SequenceIterator.create sequence 1 Int.max_value))
  ) else (
    Mailbox.MailboxStorage.status Mailbox.this >>= fun mailbox_metadata ->
    if buid = false then (
      return (Some (SequenceIterator.create sequence 1 mailbox_metadata.count))
    ) else (
      Mailbox.MailboxStorage.fetch_message_metadata Mailbox.this (`Sequence 1) >>= function
      | `Ok message_metadata ->
        return (Some (SequenceIterator.create sequence message_metadata.uid
          (mailbox_metadata.uidnext - 1)))
      | _ -> return None
    )
  )

let iter_selected_with_seq mailboxt sequence buid f =
  let open Interpreter in
  match (selected_mbox mailboxt) with
  | None -> return (`Error "Not selected")
  | Some mailbox ->
  factory mailboxt mailbox >>= fun (module Mailbox) ->
  Mailbox.MailboxStorage.exists Mailbox.this >>= function
  | `No -> return (`NotExists)
  | `Folder -> return (`NotSelectable)
  | `Mailbox -> 
    get_iterator (module Mailbox) buid sequence >>= function
    | None -> return `Ok
    | Some it ->
      let open Seq_iterator in
      let rec read = function
        | `End -> return ()
        | `Ok seq ->
          let pos = if buid then (`UID seq) else (`Sequence seq) in
          f (module Mailbox : Storage.Storage_inst) pos seq >>= function
          | `Eof -> return ()
          | `Ok -> read (SequenceIterator.next it)
      in
      read (SequenceIterator.next it) >> return `Ok

(* fetch data from mailbox *)
let fetch mailboxt resp_writer sequence fetchattr buid =
  iter_selected_with_seq mailboxt sequence buid (fun (module Mailbox) pos seq ->
    Mailbox.MailboxStorage.fetch Mailbox.this pos >>= function
    | `Eof -> return `Eof
    | `NotFound -> return `Ok
    | `Ok (message,metadata) -> 
      (* need more efficient exec_fetch since sequence is already tested
       by the iterator *)
      let res = Interpreter.exec_fetch seq sequence message metadata fetchattr buid in
      match res with
      | Some res -> resp_writer res; return `Ok
      | None -> return `Ok
  )

(* store flags *)
let store mailboxt resp_writer sequence storeattr flagsval buid =
  iter_selected_with_seq mailboxt sequence buid (fun (module Mailbox) pos seq ->
    Mailbox.MailboxStorage.fetch_message_metadata Mailbox.this pos >>= function
    | `Eof -> return `Eof
    | `NotFound -> return `Ok
    | `Ok message_metadata ->
      match Interpreter.exec_store message_metadata seq sequence storeattr flagsval buid with
      | `None -> return `Ok
      | `Silent metadata -> 
        Mailbox.MailboxStorage.store Mailbox.this pos metadata >>
        Mailbox.MailboxStorage.commit Mailbox.this >>
        return `Ok
      | `Ok (metadata,res) -> 
        resp_writer res >>
        Mailbox.MailboxStorage.store Mailbox.this pos metadata >>
        Mailbox.MailboxStorage.commit Mailbox.this >>
        return `Ok
  )

(* copy messages to mailbox *)
let copy mailboxt dest_mbox sequence buid =
  let open Core.Std in
  match (selected_mbox mailboxt) with
  | None -> return (`Error "Not selected")
  | Some src_mbox ->
  factory mailboxt ?mailbox2:(Some dest_mbox) src_mbox >>= fun (module Mailbox) ->
  Mailbox.MailboxStorage.exists (Option.value_exn Mailbox.this2) >>= function
  | `No -> return (`NotExists)
  | `Folder -> return (`NotSelectable)
  | `Mailbox -> 
    Mailbox.MailboxStorage.copy Mailbox.this (Option.value_exn Mailbox.this2) sequence buid >>
    Mailbox.MailboxStorage.commit Mailbox.this >>
    Mailbox.MailboxStorage.commit (Option.value_exn Mailbox.this2) >>
    return `Ok

(* permanently remove messages with \Deleted flag *)
let expunge mailboxt resp_writer =
  match (selected_mbox mailboxt) with
  | None -> return (`Error "Not selected")
  | Some mailbox ->
    factory mailboxt mailbox >>= fun (module Mailbox) ->
    Mailbox.MailboxStorage.expunge Mailbox.this (fun seq ->
      resp_writer ((string_of_int seq) ^ " EXPUNGE")
    ) >>
    Mailbox.MailboxStorage.commit Mailbox.this >>
    return `Ok
