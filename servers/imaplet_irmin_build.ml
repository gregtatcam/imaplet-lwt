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
open Imaplet
open Commands
open Imaplet_types
open Regex
open Storage_meta
open Irmin_core
open Irmin_storage
open Server_config
open Parsemail

exception InvalidCommand
exception PasswordRequired
exception InvalidInput
exception Done

module MapChar = Map.Make(Char)
module MapStr = Map.Make(String)

let timer = ref 0.

let timer_start () =
  timer := !timer -. Unix.gettimeofday ()

let timer_stop () =
  timer := !timer +. Unix.gettimeofday ()

let with_timer f =
  timer_start (); 
  f() >>= fun res ->
  timer_stop ();
  return res

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

let get_filter str =
  let p = Str.split (Str.regexp ":") str in
  List.fold_left (fun (start,max,folders) n ->
    if match_regex ~regx:"^\\([0-9]+\\)-\\([0-9]+\\)$" n then
      (int_of_string (Str.matched_group 1 n),int_of_string (Str.matched_group 2 n),folders)
    else 
      (start,max, Str.split (Str.regexp ",") n)
  ) (1,max_int,[]) p

(* -u : user
 * -m : mailbox structure
 *      mbox:inbox-path:mailboxes-path
 *      maildir:maildir-path
 *)
let rec args i user mailbox filter isappend =
  if i >= Array.length Sys.argv then
    user,mailbox,filter,isappend
  else
    match Sys.argv.(i) with 
    | "-m" -> args (i+2) user (Some (get_mailbox_structure Sys.argv.(i+1))) filter isappend
    | "-u" -> args (i+2) (Some Sys.argv.(i+1)) mailbox filter isappend
    | "-f" -> args (i+2) user mailbox (get_filter Sys.argv.(i+1)) isappend
    | "-a" -> args (i+1) user mailbox filter true
    | _ -> raise InvalidCommand

let usage () =
  Printf.printf "usage: imaplet_irmin_build -u [user:pswd] -m
  [mbox:inbox-path:mailboxes-path|maildir:mailboxes-path|archive:mbox-path]
  -f [start-maxmsg:folder,...,folder] -a\n%!"

let get_user_pswd user isappend =
  if Regex.match_regex ~regx:"^\\([^:]+\\):\\(.+\\)$" user then
    ((Str.matched_group 1 user),Some (Str.matched_group 2 user))
  else if srv_config.auth_required || isappend = false then
    raise PasswordRequired
  else
    (user,None)

let get_user (u,p) =
  u

let commands f =
  try 
    let user,mbx,filter,isappend = args 1 None None (1,max_int,[]) false in
    if user = None || mbx = None then
      usage ()
    else
      try 
        let up = get_user_pswd (Utils.option_value_exn user) isappend in
        f up (Utils.option_value_exn mbx) filter isappend 
      with ex -> Printf.printf "%s\n%!" (Printexc.to_string ex)
  with 
  | PasswordRequired -> Printf.printf "password is required: -u user:pswd\n%!"
  | _ -> usage ()

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

let user_keys = ref None

let get_keys (user,pswd) =
  match !user_keys with 
  | None ->
    Ssl_.get_user_keys ~user ?pswd srv_config >>= fun keys ->
    user_keys := Some keys;
    return keys
  | Some keys -> return keys

let create_mailbox user ?uidvalidity mailbox =
  Printf.printf "############# creating mailbox %s %s\n%!" (get_user user) mailbox;
  with_timer (fun() -> 
  get_keys user >>= fun keys ->
  IrminStorage.create srv_config (get_user user) mailbox keys >>= fun ist ->
  IrminStorage.create_mailbox ist >>
  return ist) >>= fun ist ->
  match uidvalidity with
  | Some uidvalidity ->
    let metadata = empty_mailbox_metadata ~uidvalidity () in
    IrminStorage.store_mailbox_metadata ist metadata >>
    return ist
  | None -> return ist

let append ist ?uid message size flags mailbox =
  catch (fun () ->
  with_timer (fun() -> IrminStorage.status ist) >>= fun mailbox_metadata ->
  let modseq = Int64.add mailbox_metadata.modseq Int64.one in
  let uid = match uid with None -> mailbox_metadata.uidnext|Some uid -> uid in
  let message_metadata = {
    uid;
    modseq;
    size;
    internal_date = Dates.ImapTime.now();
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
  with_timer (fun() -> 
  IrminStorage.store_mailbox_metadata ist mailbox_metadata >>
  IrminStorage.append ist message message_metadata >>
  IrminStorage.commit ist)
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
      | "Unread" -> 
        (label,inferred,Flags_Recent :: ((List.filter (fun f -> f <> Flags_Seen)) flags))
      | "Draft" -> (label,(Some "Drafts"),Flags_Draft :: flags)
      | label -> ((Some label),inferred, flags)
    ) (None,None,[Flags_Seen]) labels
    in
    match label with
    | None ->
      begin
      match inferred with
      | None -> ("INBOX",flags)
      | Some label -> (label,flags)
      end
    | Some label ->
      let label = Regex.replace ~regx:"\\[Imap\\]/" ~tmpl:"" label in
      let label = Regex.replace ~case:false ~regx:"inbox" ~tmpl:"INBOX" label in
      (label,flags)
  )

let append_messages ist path flags =
  Printf.printf "#### appending messages %s\n%!" path;
  let wseq = Mailbox.With_seq.t_of_file path in
  Mailbox.With_seq.fold_message wseq ~f:(fun _ (message:Mailbox.Message.t) ->
    if Regex.match_regex (Mailbox.Postmark.to_string message.postmark) ~regx:"^From[ ]+MAILER_DAEMON" then 
      return ()
    else (
      let size = String.length (Email.to_string message.email) in
      append ist message size flags ""
    )
  ) ~init:(return())
  
let gmail_mailboxes = ref MapStr.empty;;
gmail_mailboxes := MapStr.add "INBOX" "" !gmail_mailboxes;;
gmail_mailboxes := MapStr.add "Drafts" "" !gmail_mailboxes;;
gmail_mailboxes := MapStr.add "Deleted Messages" "" !gmail_mailboxes;;
gmail_mailboxes := MapStr.add "Sent Messages" "" !gmail_mailboxes;;

let postmark = "^from [^ ]+ " ^ Regex.dayofweek ^ " " ^ Regex.mon ^ " " ^
  "[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [0-9]+"

let is_postmark line =
  Regex.match_regex ~case:false ~regx:postmark line

let filter_folder folders f =
  let lowercase s = String.lowercase s in
  if folders = [] then
    false
  else
    List.exists (fun n -> (lowercase (Regex.dequote n)) = (lowercase f)) folders = false

let parse_message content =
  let wseq = Mailbox.With_seq.of_string content in
  Mailbox.With_seq.fold_message wseq ~f:(fun _ message ->
    let headers = String_monoid.to_string (Header.to_string_monoid (Email.header message.email)) in
    let (mailbox,fl) = mailbox_of_gmail_label headers in
    let size = String.length (Email.to_string message.email) in
    (Some (mailbox,size,fl,message))
  ) ~init:None

let rec get_message ic buffer folders =
  let parse_content content folders f =
    match parse_message content with
    | None -> f ()
    | Some (mailbox,size,flags,message) ->
      if filter_folder folders mailbox then (
        f ()
      ) else (
        return (Some (mailbox,size,flags,message))
      )
  in
  Lwt_io.read_line_opt ic >>= function
  | None -> 
    if Buffer.length buffer > 0 then
      let content = Buffer.contents buffer in
      Buffer.clear buffer;
      parse_content content folders (fun () -> return None)
    else
      return None
  | Some line ->
    let line = line ^ "\n" in
    if is_postmark line then (
      let content = Buffer.contents buffer in
      Buffer.clear buffer;
      Buffer.add_string buffer line;
      parse_content content folders (fun () -> get_message ic buffer folders)
    ) else (
      Buffer.add_string buffer line;
      get_message ic buffer folders
    )

let fill path push_strm start maxmsg folders =
  Lwt_io.with_file ~mode:Lwt_io.Input path (fun ic ->
    let buffer = Buffer.create 1000 in
    let rec loop cnt =
      get_message ic buffer folders >>= function
      | None -> push_strm None; return ()
      | Some (mailbox,size,flags,message) ->
        if cnt >= start then
          push_strm (Some (cnt,mailbox,size,flags,message));
        if cnt < maxmsg then
          loop (cnt + 1)
        else (
          push_strm None;
          return ()
        )
    in
    loop 1
  )

let populate_mailboxes user isappend =
  if isappend then (
    Printf.printf "#### fetching current mailboxes\n%!";
    get_keys user >>= fun keys ->
    IrminStorage.create srv_config (get_user user) "" keys >>= fun ist ->
    IrminStorage.list ~subscribed:true ~access:(fun _ -> true) ist ~init:() ~f:(fun _ f ->
      match f with
      | `Mailbox (f,s) -> Printf.printf "mailbox: %s\n%!" f;gmail_mailboxes := MapStr.add f "" !gmail_mailboxes; return ()
      | `Folder (f,s) -> Printf.printf "folder: %s\n%!" f;gmail_mailboxes := MapStr.add f "" !gmail_mailboxes; return ()
    ) 
  ) else
    return ()

let append_archive_messages user path filter flags isappend =
  Printf.printf "#### appending archive messages %s\n%!" path;
  populate_mailboxes user isappend >>= fun () ->
  let (start,maxmsg,folders) = filter in
  let (strm,push_strm) = Lwt_stream.create () in
  async (fun () -> fill path push_strm start maxmsg folders);
  Lwt_stream.fold_s (fun (cnt,mailbox,size,fl,message) _ ->
    Printf.printf "-- processing message %d, mailbox %s\n%!" cnt mailbox;
    begin
    if MapStr.exists (fun mb _ -> mailbox = mb) !gmail_mailboxes then
      get_keys user >>= fun keys ->
      with_timer (fun() -> IrminStorage.create srv_config (get_user user) mailbox keys)
    else (
      gmail_mailboxes := MapStr.add mailbox "" !gmail_mailboxes;
      create_mailbox user mailbox
    )
    end >>= fun ist ->
    append ist message size (List.concat [flags;fl]) mailbox >>
    return ()
 ) strm ()

let append_maildir_message ist ?uid path flags =
  Printf.printf "#### appending maildir message %s\n%!" path;
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
      let msgflags =
      if Regex.match_regex name ~regx:":2,\\([a-zA-Z]+\\)$" then (
        let flags = Str.matched_group 1 name in
        let rec fold str i acc =
          try
            let flag = str.[i] in
            try 
              let flag = MapChar.find flag flagsmap in
              fold str (i+1) (flag :: acc)
            with _ -> fold str (i+1) acc
          with _ -> acc in
        fold flags 0 []
      ) else (
        []
      )
      in
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
    ) acc
  in
  populate ist (Filename.concat path "new") [Flags_Recent] flagsmap uidmap >>
  populate ist (Filename.concat path "cur") [] flagsmap uidmap 

let create_inbox user inbx =
  Printf.printf "creating mailbox: INBOX\n%!";
  create_mailbox user "INBOX" >>= fun ist ->
  populate_mbox_msgs ist inbx >>
  with_timer(fun() -> IrminStorage.commit ist)

let create_account user subscriptions =
  let (u,p) = user in
  Printf.printf "#### creating user account %s\n%!" (get_user user);
  Lwt_unix.system ("imaplet_create_account -u " ^ u ^ ":" ^ (Utils.option_value_exn p)) >>= fun _ ->
  (* get subscriptions - works for dovecot *)
  get_keys user >>= fun keys ->
  catch (fun () ->
    let strm = Lwt_io.lines_of_file subscriptions in
    Lwt_stream.iter_s (fun line -> 
      IrminStorage.create srv_config (get_user user) line keys >>= fun ist ->
      IrminStorage.subscribe ist >>
      IrminStorage.commit ist
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
let create_mbox user inbox mailboxes filter =
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
    with_timer(fun() -> IrminStorage.commit ist)
  )

let maildir_flags flagsmap =
  let keys = ['P';'R';'S';'T';'D';'F'] in
  let values =
    [Flags_Answered;Flags_Answered;Flags_Seen;Flags_Deleted;Flags_Draft;Flags_Flagged] in
  let (_,flagsmap) =
  List.fold_left (fun (i,flagsmap) key ->
    i+1, MapChar.add key (List.nth values i) flagsmap
  ) (0,flagsmap) keys
  in
  flagsmap

let create_maildir user mailboxes fs filter =
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
      with_timer(fun() -> IrminStorage.commit ist)
    )
    ( fun ex -> Printf.fprintf stderr "create_maildir exception: %s %s\n%!" (Printexc.to_string ex)
    (Printexc.get_backtrace());return())
  )

let create_archive_maildir user mailbox filter isappend =
  append_archive_messages user mailbox filter [] isappend  

let () =
  commands (fun user mbx filter isappend ->
    Lwt_main.run (
      catch ( fun () ->
        match mbx with
        | `Mbox (inbox,mailboxes) -> 
          Printf.printf "porting from mbox\n%!";
          (if isappend then return () else create_account user (Filename.concat mailboxes ".subscriptions")) >>
          create_mbox user inbox mailboxes filter 
        | `Maildir (mailboxes,fs) -> 
          Printf.printf "porting from maildir\n%!";
          (if isappend then return () else create_account user (Filename.concat mailboxes "subscriptions")) >>
          create_maildir user mailboxes fs filter 
        | `Archive mailbox ->
          Printf.printf "porting from archive\n%!";
          (if isappend then return () else create_account user (Filename.concat mailbox "subscriptions")) >>
          create_archive_maildir user mailbox filter isappend
      )
      (fun ex -> Printf.fprintf stderr "exception: %s %s\n%!" (Printexc.to_string ex)
      (Printexc.get_backtrace());return())
    )
  );
  Printf.printf "total irmin time: %04f\n%!" !timer
