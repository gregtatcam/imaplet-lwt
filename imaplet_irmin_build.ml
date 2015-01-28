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
open Irmin_core
open Irmin_storage

exception InvalidCommand
exception InvalidInput
exception Done

module MapChar = Map.Make(Char)
module MapStr = Map.Make(String)

let get_mailbox_structure str =
  if match_regex str ~regx:"^mbox:\\([^:]+\\):\\(.+\\)$" then
    `Mbox (Str.matched_group 1 str,Str.matched_group 2 str)
  else if match_regex str ~regx:"^maildir:\\([^:]+\\)\\(:fs\\)?$" then (
    let dir = (Str.matched_group 1 str) in
    let fs = try let _ = Str.matched_group 2 str in true with _ -> false in
    `Maildir (dir,fs)
  ) else if match_regex str ~regx:"^archive:\\([^:]+\\)$" then (
    `Archive (Str.matched_group 1 str)
  ) else 
    raise InvalidCommand

(* -u : user
 * -m : mailbox structure
 *      mbox:inbox-path:mailboxes-path
 *      maildir:maildir-path
 *)
let rec args i user mailbox =
  if i >= Array.length Sys.argv then
    user,mailbox
  else
    match Sys.argv.(i) with 
    | "-m" -> args (i+2) user (Some (get_mailbox_structure Sys.argv.(i+1)))
    | "-u" -> args (i+2) (Some Sys.argv.(i+1)) mailbox
    | _ -> raise InvalidCommand

let usage () =
  Printf.printf "usage: imaplet_irmin_build -u [user] -m
  [mbox:inbox-path:mailboxes-path|maildir:mailboxes-path|archive:mbox-path]\n%!"

let commands f =
  try 
    let user,mbx = args 1 None None in
    if user = None || mbx = None then
      usage ()
    else
      try 
        f (Utils.option_value_exn user) (Utils.option_value_exn mbx)
      with ex -> Printf.printf "%s\n%!" (Printexc.to_string ex)
  with _ -> usage ()

let rec listdir path mailbox f =
  Printf.printf "listing %s\n%!" path;
  let strm = Lwt_unix.files_of_directory path in
  Lwt_stream.iter_s (fun i -> 
    if i = "." || i = ".." || i = ".imap" || i = ".imaplet" || i = ".subscriptions" then
      return ()
    else (
      let path = Filename.concat path i in
      let mailbox = Filename.concat mailbox i in
      Lwt_unix.stat path >>= fun stat ->
      if stat.Lwt_unix.st_kind = Lwt_unix.S_DIR then
        f true path mailbox >>
        listdir path mailbox f 
      else
        f false path mailbox
    )
  ) strm 

let create_mailbox user ?uidvalidity mailbox =
  Printf.printf "############# creating mailbox %s %s\n%!" user mailbox;
  IrminStorage.create user mailbox >>= fun ist ->
  IrminStorage.create_mailbox ist >>
  match uidvalidity with
  | Some uidvalidity ->
    let metadata = empty_mailbox_metadata ~uidvalidity () in
    IrminStorage.store_mailbox_metadata ist metadata >>
    return ist
  | None -> return ist

let append ist ?uid message size flags mailbox =
  catch (fun () ->
  IrminStorage.status ist >>= fun mailbox_metadata ->
  let modseq = Int64.add mailbox_metadata.modseq Int64.one in
  let uid = match uid with None -> mailbox_metadata.uidnext|Some uid -> uid in
  let message_metadata = {
    uid;
    modseq;
    size;
    internal_date = Dates.ImapTime.epoch;
    flags;
  } in
  let mailbox_metadata = { mailbox_metadata with
    uidnext = uid + 1;
    count = mailbox_metadata.count + 1;
    recent = mailbox_metadata.recent + 1;
    unseen = 
      if mailbox_metadata.unseen = 0 then
        mailbox_metadata.count + 1  
      else
        mailbox_metadata.unseen
    ;
    nunseen = mailbox_metadata.nunseen + 1 ;
    modseq
  } in
  IrminStorage.store_mailbox_metadata ist mailbox_metadata >>
  IrminStorage.append ist message message_metadata 
  )
  (fun ex -> Printf.fprintf stderr "append exception: %s %s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace()); return ())

let mailbox_of_gmail_label message = 
  if Regex.match_regex message ~regx:"^X-Gmail-Labels: \\(.+\\)$" = false then 
    ("INBOX",[])
  else (
    let labels = Str.split (Str.regexp ",") (Str.matched_group 1 message) in
    let (label,inferred,flags) =
    List.fold_left (fun (label,inferred,flags) l ->
      match l with 
      | "Important" -> (label,inferred,(Flags_Keyword "Important") :: flags)
      | "Starred" -> (label,inferred,(Flags_Keyword "Starred") :: flags)
      | "Sent" -> (label,(Some "Sent Messages"),Flags_Answered :: flags)
      | "Trash" -> (label,(Some "Deleted Messages"),Flags_Deleted :: flags)
      | "Unread" -> (label,inferred,Flags_Recent :: flags)
      | "Draft" -> (label,(Some "Drafts"),Flags_Draft :: flags)
      | label -> ((Some label),inferred, flags)
    ) (None,None,[]) labels
    in
    match label with
    | None ->
      begin
      match inferred with
      | None -> ("INBOX",flags)
      | Some label -> (label,flags)
      end
    | Some label ->
      let label = Regex.replace ~regx:"[Imap]/" ~tmpl:"" label in
      let label = Regex.replace ~case:false ~regx:"inbox" ~tmpl:"INBOX" label in
      (label,flags)
  )

let append_messages ist path flags =
  Printf.printf "#### appending messages %s\n%!" path;
  let open Email_message in
  let open Email_message.Mailbox in
  let wseq = With_seq.t_of_file path in
  With_seq.fold_message wseq ~f:(fun _ (message:Mailbox.Message.t) ->
    if Regex.match_regex (Postmark.to_string message.postmark) ~regx:"^From[ ]+MAILER_DAEMON" then 
      return ()
    else (
      let size = String.length (Email.to_string message.email) in
      append ist message size flags ""
    )
  ) ~init:(return())
  
let gmail_mailboxes = ref MapStr.empty

let append_archive_messages user path flags =
  Printf.printf "#### appending archive messages %s\n%!" path;
  let open Email_message in
  let open Email_message.Mailbox in
  let wseq = With_seq.t_of_file path in
  With_seq.fold_message wseq ~f:(fun acc message ->
    acc >>= fun (cnt, prev_mailbox, prev_ist) ->
    let size = String.length (Email.to_string message.email) in
    let headers = String_monoid.to_string (Header.to_string_monoid (Email.header message.email)) in
    let (mailbox,fl) = mailbox_of_gmail_label headers in
    Printf.printf "-- processing message %d, mailbox %s\n%!" cnt mailbox;
    begin
    if prev_ist = None then (
      gmail_mailboxes := MapStr.add mailbox "" !gmail_mailboxes;
      create_mailbox user mailbox 
    ) else if prev_mailbox <> mailbox then (
      IrminStorage.commit (Utils.option_value_exn prev_ist) >>
      if MapStr.exists (fun mb _ -> mailbox = mb) !gmail_mailboxes then
        IrminStorage.create user mailbox
      else (
        gmail_mailboxes := MapStr.add mailbox "" !gmail_mailboxes;
        create_mailbox user mailbox
      )
    ) else (
      return (Utils.option_value_exn prev_ist)
    ) 
    end >>= fun ist ->
    append ist message size (List.concat [flags;fl]) mailbox >>
    return (cnt + 1,mailbox,Some ist)
  ) ~init:(return(1,"",None)) >>= fun (_,_,ist) ->
  match ist with
  | None -> return ()
  | Some ist -> IrminStorage.commit ist

let append_maildir_message ist ?uid path flags =
  Printf.printf "#### appending maildir message %s\n%!" path;
  let open Email_message.Mailbox in
  Lwt_io.file_length path >>= fun size ->
  let size = Int64.to_int size in
  let buffer = String.create size in
  Lwt_io.open_file ~mode:Lwt_io.Input path >>= fun ic ->
  Lwt_io.read_into_exactly ic buffer 0 size >>= fun () ->
  Lwt_io.close ic >>= fun () ->
  if Regex.match_regex buffer ~regx:"^From[ ]+MAILER_DAEMON" then 
    return ()
  else (
    let message = Utils.make_email_message buffer in
    append ist ?uid message size flags ""
  )

let populate_mbox_msgs ist path =
  append_messages ist path [Flags_Recent]

(* maildir directory structure is flat *)
let listdir_maildir path f =
  let strm = Lwt_unix.files_of_directory path in
  Lwt_stream.fold_s (fun i acc -> 
    if i = "." || i = ".." || Regex.match_regex i ~regx:"^\\..+[^.]$" = false then
      return acc
    else (
      return (i :: acc)
    )
  ) strm [".INBOX"] >>= fun acc ->
  let acc = List.sort String.compare acc in
  Lwt_list.iter_s (fun d ->
    let dirs = Str.split (Str.regexp "\\.") d in
    let mailbox = "/" ^ (String.concat "/" dirs) in
    let path = if d = ".INBOX" then path else Filename.concat path d in
    f false path mailbox
  ) acc

let populate_maildir_msgs ist path flagsmap uidmap =
  Printf.printf "#### populating maildir %s\n%!" path;
  let populate ist path initflags flagsmap uidmap =
    let strm = Lwt_unix.files_of_directory path in
    Lwt_stream.fold_s (fun i acc -> 
      if i = "." || i = ".." then
        return acc
      else (
        return (i :: acc)
      )
    ) strm [] >>= fun acc ->
    (* sort by file name - hopefully it'll sort in chronological order based on the
     * unique id
     *)
    Printf.printf "#### number of messages in %s  %d\n%!" path (List.length acc);
    let acc = List.sort String.compare acc in
    Lwt_list.iter_s (fun name -> 
      Printf.printf "#### processing %s\n%!" name;
      let (trash,msgflags) =
      if Regex.match_regex name ~regx:":2,\\([a-zA-Z]+\\)$" then (
        let flags = Str.matched_group 1 name in
        let rec fold str i (trash,acc) =
          try
            let flag = str.[i] in
            if flag = 'T' then
              (true,acc)
            else
              try 
                let flag = MapChar.find flag flagsmap in
                fold str (i+1) (trash,flag :: acc)
              with _ -> fold str (i+1) (trash,acc)
          with _ -> (trash,acc) in
        fold flags 0 (false,[])
      ) else (
        false,[]
      )
      in
      if trash then
        return ()
      else (
        let flags = List.concat [initflags;msgflags] in
        let uid = 
          if match_regex name ~regx:"^\\([^:]+\\)" then
            try
              Some (MapStr.find (Str.matched_group 1 name) uidmap)
            with _ -> None
          else
            None
        in
        append_maildir_message ist ?uid (Filename.concat path name) flags
      )
    ) acc
  in
  populate ist (Filename.concat path "new") [Flags_Recent] flagsmap uidmap >>
  populate ist (Filename.concat path "cur") [] flagsmap uidmap 

let create_inbox user inbx =
  Printf.printf "creating mailbox: INBOX\n%!";
  create_mailbox user "INBOX" >>= fun ist ->
  populate_mbox_msgs ist inbx >>
  IrminStorage.commit ist

let create_account user subscriptions =
  let ac = UserAccount.create user in
  UserAccount.delete_account ac >>= fun _ ->
  UserAccount.create_account ac >>= fun _ ->
  (* get subscriptions - works for dovecot *)
  Subscriptions.create user >>= fun s ->
  catch (fun () ->
    let strm = Lwt_io.lines_of_file subscriptions in
    Lwt_stream.iter_s (fun line -> 
      Subscriptions.subscribe s line
    ) strm
  ) (fun _ -> return ())

let get_dovecot_params path =
  catch (fun () ->
  let alpha = "abcdefghijklmnopqrstuvwxyz" in
  let strm = Lwt_io.lines_of_file (Filename.concat path "dovecot-keywords") in
  Lwt_stream.fold_s (fun line acc -> 
    if match_regex line ~regx:"^\\([0-9]+\\) \\(.+\\)$" then (
      let i = int_of_string (Str.matched_group 1 line) in
      if i >= 0 && i <= 26 then (
        let key = String.get alpha i in
        let data = Flags_Keyword (Str.matched_group 2 line) in
        return (MapChar.add key data acc)
      ) else
        return acc
    ) else
      return acc
  ) strm (MapChar.empty)
  ) (fun _ -> return (MapChar.empty)) >>= fun flagsmap ->
  catch( fun () ->
    let strm = Lwt_io.lines_of_file (Filename.concat path "dovecot-uidlist") in
    Lwt_stream.fold_s (fun line (first,uidvalidity,acc) -> 
      if first then (
       if match_regex line ~regx:"^[0-9]+ V\\([0-9]+\\) " then 
         return (false,Some (Str.matched_group 1 line),acc)
       else
         return (false, uidvalidity, acc)
      ) else if match_regex line ~regx:"^\\([0-9]+\\)[^:]:\\(.+\\)$" then (
        let key = Str.matched_group 2 line in
        let data = int_of_string (Str.matched_group 1 line) in
        return (first,uidvalidity,MapStr.add key data acc)
      ) else
       raise InvalidInput
    ) strm (true,None,MapStr.empty) >>= fun (_,uidvalidity,uidmap) ->
    return (flagsmap,uidvalidity,uidmap)
  )
  (fun _ -> return (flagsmap,None,MapStr.empty))

(* irmin supports both mailbox and folders under the mailbox *)
let create_mbox user inbox mailboxes =
  create_inbox user inbox >>
  listdir mailboxes "/" (fun is_dir path mailbox ->
    Printf.printf "creating mailbox: %s\n%!" mailbox;
    create_mailbox user mailbox >>= fun ist ->
    begin
    if is_dir then
      return ()
    else
      populate_mbox_msgs ist path
    end >>
    IrminStorage.commit ist
  )

let maildir_flags flagsmap =
  let keys = ['P';'R';'S';'D';'F'] in
  let values =
    [Flags_Answered;Flags_Answered;Flags_Seen;Flags_Draft;Flags_Flagged] in
  let (_,flagsmap) =
  List.fold_left (fun (i,flagsmap) key ->
    i+1, MapChar.add key (List.nth values i) flagsmap
  ) (0,flagsmap) keys
  in
  flagsmap

let create_maildir user mailboxes fs =
  let list = if fs then listdir mailboxes "/" else listdir_maildir mailboxes in
  list (fun _ path mailbox ->
    Printf.printf "#### listing %s %s\n%!" path mailbox;
    catch (fun () ->
      get_dovecot_params path >>= fun (flagsmap,uidvalidity,uidmap) ->
      let flagsmap = maildir_flags flagsmap in
      Printf.printf "#### got dovecot params %d %d %s\n%!" (MapChar.cardinal
      flagsmap) (MapStr.cardinal uidmap) (Utils.option_value uidvalidity ~default:"");
      create_mailbox user ?uidvalidity mailbox >>= fun ist ->
      populate_maildir_msgs ist path flagsmap uidmap >>= fun () ->
      IrminStorage.commit ist
    )
    ( fun ex -> Printf.fprintf stderr "create_maildir exception: %s %s\n%!" (Printexc.to_string ex)
    (Printexc.get_backtrace());return())
  )

let create_archive_maildir user mailbox =
  append_archive_messages user mailbox []

let () =
  commands (fun user mbx ->
    Lwt_main.run (
      catch ( fun () ->
        match mbx with
        | `Mbox (inbox,mailboxes) -> 
          Printf.printf "porting from mbox\n%!";
          create_account user (Filename.concat mailboxes ".subscriptions") >>
          create_mbox user inbox mailboxes
        | `Maildir (mailboxes,fs) -> 
          Printf.printf "porting from maildir\n%!";
          create_account user (Filename.concat mailboxes "subscriptions") >>
          create_maildir user mailboxes fs
        | `Archive mailbox ->
          Printf.printf "porting from archive\n%!";
          create_account user (Filename.concat mailbox "subscriptions") >>
          create_archive_maildir user mailbox 
      )
      (fun ex -> Printf.fprintf stderr "exception: %s %s\n%!" (Printexc.to_string ex)
      (Printexc.get_backtrace());return())
    )
  )
