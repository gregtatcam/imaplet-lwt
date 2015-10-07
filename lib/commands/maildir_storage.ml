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
open Sexplib
open Sexplib.Conv
open Storage
open Storage_meta
open Imaplet_types
open Lazy_message
open Lazy_maildir_message
open Parsemail
open Server_config

exception EmptyPrivateKey

module MapStr = Map.Make(String)
module MapFlag = Map.Make(
  struct
    type t = mailboxFlags
    let compare f1 f2 = Pervasives.compare f1 f2
  end)

let _mail_path config user =
  Utils.user_path ~path:config.Server_config.mail_path ~user ()

module MaildirPath : sig 
  type t
  val create : Server_config.imapConfig -> string -> string -> t
  val to_maildir : t -> string
  val to_unix : t -> string
  val to_unix_path : string -> string
  val file_path : t -> [`Cur of string|`Tmp of string|`New of
  string|`Metadata|`Uidlist|`Keywords] -> string
  val file_path_of_maildir : t -> string -> [`Cur of string|`Tmp of string|`New of
  string|`Metadata|`Uidlist|`Keywords] -> string
  val basename_of_maildir : string -> string
  val basename : t -> string
  val dirname_of_maildir : string -> string
  val dirname : t -> string
  val mailbox : t -> string
  val root : t -> string
  val trim_mailbox : t -> string -> string
end = struct
  (* root * mailbox 
   * root is location of Maildir, for inst. /User/dovecot/Maildir
   * mailbox is relative to the root
   *)
  type t = {root:string;mailbox:string;config:Server_config.imapConfig}

  let mailbox t =
    t.mailbox

  let root t =
    t.root

  (* .Foo.Foo1.Foo2 -> Foo/Foo1/Foo2
   * maildir structure is flat with subfolders separated by "."
   * imaplet internally maintains Unix mailbox path
   *)
  let maildir_path_to_unix path =
   let path = Regex.replace ~regx:"^\\." ~tmpl:"" path in
   Regex.replace ~regx:"\\." ~tmpl:"/" path

  (* convert unix path to maildir format Foo/Foo1/Foo2 -> .Foo.Foo1.Foo2 *)
  let unix_path_to_maildir path =
   let path = Regex.replace ~regx:"^/" ~tmpl:"" path in
   let path = Regex.replace ~regx:"/" ~tmpl:"." path in
   "." ^ path
  
  (* mailbox is in unix format i.e. foo/foo1/foo2
   * will change to .foo.foo1.foo2 *)
  let create config user mailbox =
    let open Server_config in
    let mailbox =
      let lcase = String.lowercase mailbox in
      (* inbox doesn't have it's own folder, messages are placed into
       * tmp/cur/new under directly under Maildir
       *)
      if lcase = ".inbox" || lcase = "inbox" || mailbox = "." || mailbox = "" then
        ""
      else
        unix_path_to_maildir mailbox
    in
    {root=Configuration.mailboxes (_mail_path config user) user;mailbox;config}

  (* return full mailbox path formated for use by OS *)
  let to_maildir t =
    if t.mailbox <> "" then
      Filename.concat t.root t.mailbox
    else
      t.root

  (* only convert the mailbox part to the unix format *)
  let to_unix t =
    maildir_path_to_unix t.mailbox

  let to_unix_path mailbox =
    maildir_path_to_unix mailbox

  (* get location of a file under the mailbox *)
  let file_path_of_maildir t maildir tp =
    let (=^) parent child = Filename.concat parent child in
    match tp with
    | `Cur file -> root t =^ maildir =^ "cur" =^ file
    | `Tmp file -> root t =^ maildir =^ "tmp" =^ file
    | `New file -> root t =^ maildir =^ "new" =^ file
    | `Metadata -> root t =^ maildir =^ "imaplet.meta"
    | `Uidlist -> root t =^ maildir =^ "imaplet.uidlst"
    | `Keywords -> root t =^ maildir =^ "imaplet.keywords"

  (* get location of a file under the mailbox *)
  let file_path t tp =
    file_path_of_maildir t t.mailbox tp

  (* like Filename.basename *)
  let basename_of_maildir mailbox =
    if mailbox = "" then
      ""
    else (
      let _ = Regex.match_regex ~regx:"\\.\\([^\\.]+\\)$" mailbox in
      Str.matched_group 1 mailbox
    )

  let basename t =
    basename_of_maildir t.mailbox

  (* like Filename.dirname *)
  let dirname_of_maildir mailbox =
    if mailbox = "" then
      ""
    else (
      let _ = Regex.match_regex ~regx:"\\.\\([^\\.]+\\)$" mailbox in
      Str.string_before mailbox (Str.match_beginning ())
    )

  let dirname t =
    dirname_of_maildir t.mailbox

  (* remove mailbox prefix from the file *)
  let trim_mailbox t file =
    let regx = Regex.replace ~regx:"\\." ~tmpl:"\\\\." t.mailbox in
    let regx = "^" ^ regx in
    Regex.replace ~regx ~tmpl:"" file
end

(* initial file name secs.rand.host *)
let init_message_file_name internal_date =
  let t = Int64.of_float ((Unix.gettimeofday())*.1000.) in
  let internal_date = 
    Int64.of_float (Dates.ImapTime.to_float internal_date) in
  let host = Unix.gethostname() in
  Random.init (Int64.to_int t);
  let r = Int64.to_string (Random.int64 t) in
  Printf.sprintf "%Ld.%0Lx.%s.%s" internal_date t r host

let mail_flags : (string*mailboxFlags) list =
  [ "a", Flags_Keyword "$NotJunk";
  "b", Flags_Keyword "NotJunk";
  "P", Flags_Answered;
  "T", Flags_Deleted;
  "D", Flags_Draft;
  "F", Flags_Flagged;
  "S", Flags_Seen;
  "R", Flags_Recent;]
   
(* get the keyword mapping, hardcoded for now TBD *)
let get_map_to_flag mailbox =
  (*let (>>) map (data,key) = MapStr.add data key map in*)
  List.fold_left (fun acc (m,f) -> MapStr.add m f acc) (MapStr.empty) mail_flags

let get_flag_to_map mailbox =
  List.fold_left (fun acc (m,f) -> MapFlag.add f m acc) (MapFlag.empty) mail_flags

let flags_to_map_str mailbox flags =
  let map = get_flag_to_map mailbox in
  let flags = List.sort Pervasives.compare (List.fold_right (fun f acc -> 
    try (MapFlag.find f map) :: acc with Not_found -> acc
  ) flags []) in 
  String.concat "" flags

let flags_of_map_str mailbox flags =
  let map = get_map_to_flag mailbox in
  let rec fold_right i acc =
    if i = String.length flags then
      acc
    else (
      let acc = try (MapStr.find (String.sub flags i 1) map) :: acc with Not_found -> acc in
      fold_right (i + 1) acc
    )
  in
  fold_right 0 []

(* create an empty file *)
let create_file ?(overwrite=false) ?(perms=0o666) path =
  let open Unix in
  let flags =
  if overwrite then
    [O_WRONLY;O_CREAT;O_TRUNC]
  else
    [O_WRONLY;O_EXCL;O_CREAT]
  in
  Lwt_unix.openfile path flags perms >>= fun fd ->
  Lwt_unix.close fd

(* get the filename containing the message *)
let make_message_file_name mailbox metadata =
  let file = init_message_file_name metadata.internal_date in
  (* need to get the keyword mapping from imaplet.keywords *)
  Printf.sprintf "%s,S=%d,M=%s:2,%s" file metadata.size 
    (Int64.to_string metadata.modseq) (flags_to_map_str mailbox metadata.flags)

(* time.internal_date.rand.host,S(size)=..,M(modseq)=..:2,[flags] *)
(* 1415570721.20f64da12ed.129054952358.dhcp-172-17-153-93.eduroam.wireless.private.cam.ac.uk,S=2514,M=0:2,Sa *)
let message_file_name_to_data mailbox file =
  let _ = Regex.match_regex
    ~regx:"^\\([^\\.]+\\)\\.\\([^,]+\\),S=\\([0-9]+\\),M=\\([0-9]+\\):2,\\(.*\\)$"
    file 
  in
  let internal = Dates.ImapTime.of_float (float_of_string (Str.matched_group 1 file) ) in
  let size = int_of_string (Str.matched_group 3 file) in
  let modseq = Int64.of_string (Str.matched_group 4 file) in
  let flags = flags_of_map_str mailbox (Str.matched_group 5 file) in
  (internal,size,modseq,flags)

(* update modeseq and flags *)
let update_message_file_name mailbox file metadata =
  let (_,size,modseq,flags) = message_file_name_to_data mailbox file in
  let _ = Regex.match_regex ~regx:"^\\([^,]+\\)" file in
  let immute = Str.matched_group 1 file in
  Printf.sprintf "%s,S=%d,M=%s:2,%s" immute size 
    (Int64.to_string metadata.modseq) (flags_to_map_str mailbox metadata.flags)

(* read mailbox metadata *)
let read_mailbox_metadata path =
  Utils.with_file ~lock:true path ~flags:[Unix.O_RDONLY] ~perms:0o660 ~mode:Lwt_io.Input 
  ~f:(fun ci ->
    Lwt_io.read ci >>= fun sexp_str ->
    return (mailbox_metadata_of_sexp (Sexp.of_string sexp_str))
  ) 

(* write mailbox metadata *)
let write_mailbox_metadata path metadata =
  Utils.with_file ~lock:true path ~flags:[Unix.O_WRONLY;Unix.O_TRUNC] ~perms:0o660 ~mode:Lwt_io.Output 
  ~f:(fun co ->
    Lwt_io.write co (Sexp.to_string (sexp_of_mailbox_metadata metadata)) >>
    Lwt_io.flush co
  ) 

(* read mailbox uidlist: uid filename *)
let read_uidlist path =
  Utils.with_file ~lock:true path ~flags:[Unix.O_RDONLY] ~perms:0o660 ~mode:Lwt_io.Input 
  ~f:(fun ci ->
    let rec read_line cnt acc =
      Lwt_io.read_line_opt ci >>= function
      | Some line ->
        let _ = Regex.match_regex ~regx:"^\\([0-9]+\\) \\(.+\\)$" line in
        let uid = int_of_string (Str.matched_group 1 line) and
            file = Str.matched_group 2 line in
        read_line (cnt + 1) ((cnt,uid,file) :: acc)
      | None -> return acc
    in
    read_line 1 []
  ) >>= fun l ->
  return (List.rev l)

(* write mailbox uidlist *)
let write_uidlist path l =
  Utils.with_file ~lock:true path ~flags:[Unix.O_WRONLY;Unix.O_TRUNC] ~perms:0o660 ~mode:Lwt_io.Output 
  ~f:(fun co ->
    Lwt_list.iter_s (fun (_,uid,file) ->
      Lwt_io.write_line co (String.concat " " [string_of_int uid ; file])
    ) l
  ) 

(* append to uidlist *)
let append_uidlist path uid file =
  Utils.with_file ~lock:true path ~flags:[Unix.O_WRONLY;Unix.O_APPEND] ~perms:0o660 ~mode:Lwt_io.Output 
  ~f:(fun co ->
    Lwt_io.write_line co (String.concat " " [string_of_int uid ; file])
  ) 

let subscribe_path mail_path user =
  Filename.concat (Configuration.mailboxes (Utils.user_path ~path:mail_path ~user ()) user) "imaplet.subscribe"
  
(* read subscribe *)
let read_subscribe path =
  Utils.with_file ~lock:true path ~flags:[Unix.O_RDONLY] ~perms:0o660 ~mode:Lwt_io.Input 
  ~f:(fun ci ->
    Lwt_io.read ci >>= fun sexp_str ->
    return (list_of_sexp (fun s -> string_of_sexp s) (Sexp.of_string sexp_str))
  ) 

(* write subscribe *)
let write_subscribe path l =
  Utils.with_file ~lock:true path ~flags:[Unix.O_WRONLY;Unix.O_TRUNC] ~perms:0o660 ~mode:Lwt_io.Output 
  ~f:(fun co ->
    Lwt_io.write co (Sexp.to_string (sexp_of_list (fun s -> sexp_of_string s) l)) >>
    Lwt_io.flush co
  ) 

(* maildir storage type *)
type storage_ = {user: string; mailbox:
  MaildirPath.t;config:Server_config.imapConfig;keys:Ssl_.keys;uidlist:(int*int*string) list option ref}

module MaildirStorage : Storage_intf with type t = storage_ =
struct
  type t = storage_

  (* user *)
  let create config user mailbox keys =
    return {user;mailbox = MaildirPath.create config user
    mailbox;config;keys;uidlist=ref None}

  (* mailbox supports both folders and messages *)
  let exists t = 
    Lwt_unix.stat (MaildirPath.to_maildir t.mailbox) >>= fun st ->
    if st.Unix.st_kind = Unix.S_DIR then
      return `Mailbox
    else
      return `No

  let current t file =
    MaildirPath.file_path t.mailbox (`Cur file)

  let fetch_mailbox_metadata t =
    read_mailbox_metadata (MaildirPath.file_path t.mailbox `Metadata)

  let update_mailbox_metadata t =
    write_mailbox_metadata (MaildirPath.file_path t.mailbox `Metadata)

  let fetch_uidlist t =
    match t.!uidlist with
    | None -> 
      read_uidlist (MaildirPath.file_path t.mailbox `Uidlist) >>= fun uidlist ->
      t.uidlist := Some uidlist;
      return uidlist
    | Some uidlist -> return uidlist

  let update_uidlist t l =
    t.uidlist := Some l;
    write_uidlist (MaildirPath.file_path t.mailbox `Uidlist) l

  (* status *)
  let status t =
    fetch_mailbox_metadata t

  (* select mailbox *)
  let select t =
    status t 

  (* examine mailbox *)
  let examine t =
    status t 

  (* create mailbox *)
  let create_mailbox t =
    (* inbox doesn't have it's own folder, so don't need to create *)
    begin
    if MaildirPath.basename t.mailbox <> "" then
      Lwt_unix.mkdir (MaildirPath.to_maildir t.mailbox) 0o777
    else
      return ()
    end >>
    create_file (MaildirPath.file_path t.mailbox `Metadata ) >>
    create_file (MaildirPath.file_path t.mailbox `Uidlist ) >>
    Lwt_unix.mkdir (MaildirPath.file_path t.mailbox (`Cur "") ) 0o777 >>
    Lwt_unix.mkdir (MaildirPath.file_path t.mailbox (`New "") ) 0o777 >>
    Lwt_unix.mkdir (MaildirPath.file_path t.mailbox (`Tmp "") ) 0o777 >>
    update_mailbox_metadata t (empty_mailbox_metadata ~uidvalidity:(new_uidvalidity()) ()) >>
    update_uidlist t []

  (* delete mailbox *)
  let delete t = 
    Lwt_unix.system ("rm -rf " ^ (MaildirPath.to_maildir t.mailbox)) >>= fun _ ->
    return ()

  (* rename mailbox1 mailbox2 *)
  let rename t mailbox2 =
    Lwt_unix.rename (MaildirPath.to_maildir t.mailbox) mailbox2

  (* subscribe mailbox *)
  let subscribe t =
    let mailbox = MaildirPath.basename t.mailbox in
    let mailbox = if mailbox = "" then "inbox" else mailbox in
    read_subscribe (subscribe_path t.config.mail_path t.user) >>= fun l ->
    try 
      let _ = List.find (fun m -> m = mailbox) l in return ()
    with Not_found -> write_subscribe (subscribe_path t.config.mail_path t.user) (mailbox :: l)


  (* unsubscribe mailbox *)
  let unsubscribe t =
    let mailbox = MaildirPath.basename t.mailbox in
    let mailbox = if mailbox = "" then "inbox" else mailbox in
    read_subscribe (subscribe_path t.config.mail_path t.user) >>= fun l ->
    write_subscribe (subscribe_path t.config.mail_path t.user) (List.filter (fun m -> m <> mailbox) l)

  (* list 
   * returns list of files/folders with list of flags 
   * maildir hierarchy is flat, get the list of all "folders"
   * then figure out the children count
   *)
  let list t ~subscribed ?(access=(fun _ -> true)) ~init ~f =
    let subscription = 
      if subscribed then
        Some (read_subscribe t.user)
      else 
        None
    in
    (* remove maildir root from the mailbox path *)
    let mailbox = MaildirPath.mailbox t.mailbox in
    (* maildir structure is flat; mailboxes start with ".", which is also the
     * separator; start listing with the maildir root and match against the
     * starting mailbox; the match is for subdirectories, so the starting
     * mailbox itself is not included 
     *)
    let strm = Lwt_unix.files_of_directory (MaildirPath.root t.mailbox) in
    Lwt_stream.fold_s (fun file (counts,acc) -> 
      let regx = if mailbox = "" then "" else mailbox ^ "." in
      let regx = Regex.replace ~regx:"\\.\\.$" ~tmpl:"." regx in
      let regx = Regex.replace ~regx:"\\." ~tmpl:"\\\\." regx in
      let regx = "^" ^ regx in
      if file = "." || file = ".." || file.[0] <> '.' || Regex.match_regex ~regx file = false then (
        return (counts,acc)
      ) else (
        (* the mailbox has to match exactly, i.e. if the mailbox is Test and
        * the file is Test1 then it's not a match, pattern match is done in the
        * caller
        *)
        read_mailbox_metadata (MaildirPath.file_path_of_maildir t.mailbox file `Metadata) >>= fun metadata ->
        (* need to handle subscriptions TBD *)
        if access file then (
          let counts = MapStr.add file metadata.count counts in
          let dirname = MaildirPath.dirname_of_maildir file in
          let counts =
          if dirname <> "." && dirname <> "" then
            let cnt = try MapStr.find dirname counts with Not_found -> 0 in
            MapStr.add dirname (cnt + 1) counts
          else
            counts
          in
          return (counts,file :: acc)
        ) else (
          return (counts,acc)
        )
      )
    ) strm (MapStr.empty,[]) >>= fun (counts,mailboxes) ->
    Lwt_list.fold_right_s (fun file acc ->
      let cnt = try MapStr.find file counts with Not_found -> 0 in
      let file = MaildirPath.trim_mailbox t.mailbox file in
      f acc (`Mailbox (MaildirPath.to_unix_path file,cnt)) 
    ) mailboxes init

  let encrypt t message =
    let (pub_key,_) = t.keys in
    if t.config.encrypt then (
      Imap_crypto.encrypt ~compress:t.config.compress message pub_key
    ) else if t.config.compress then (
      Imap_crypto.do_compress message
    ) else (
      message
    )

  let decrypt t message =
    let (_,priv_key) = t.keys in
    let priv_key = Utils.option_value_exn ~ex:EmptyPrivateKey priv_key in
    if t.config.encrypt then (
      Imap_crypto.decrypt ~compressed:t.config.compress message priv_key
    ) else if t.config.compress then (
      Imap_crypto.do_uncompress message
    ) else ( 
      message
    )

  (* write message to the file *)
  let write_message t file message =
    let tmp_file = MaildirPath.file_path t.mailbox (`Tmp file) in
    Utils.with_file tmp_file ~flags:[Unix.O_CREAT;Unix.O_WRONLY] ~perms:0o660 ~mode:Lwt_io.Output
    ~f:(fun oc ->
      begin
        if t.config.maildir_parse then (
          Email_parse.message_to_blob t.config t.keys message >>= fun (msg_hash, message) ->
          return message
        ) else (
          (* email parser expect \n, need to fix in the parser TBD *)
          let message = Re.replace_string (Re_posix.compile_pat "\r\n") ~by:"\n" message in
          return (encrypt t message)
        )
      end >>= fun message ->
      Lwt_io.write oc message
    ) >>= fun () ->
    let cur_file = current t file in
    Lwt_unix.link tmp_file cur_file >>
    Lwt_unix.unlink tmp_file
  
  (* append message(s) to selected mailbox *)
  let append t message message_metadata =
    let file = make_message_file_name (MaildirPath.to_maildir t.mailbox) message_metadata in
    write_message t file message >>= fun () ->
    t.uidlist := None;
    append_uidlist (MaildirPath.file_path t.mailbox `Uidlist) message_metadata.uid file 

  (* return sequence,uid,file size,file name  *)
  let get_file t position uids = 
    let len = List.length uids in
    if len = 0 then return `Eof
    else 
    begin
    let size t seq uid file =
      catch (fun () ->
        Lwt_unix.stat (current t file) >>= fun st ->
        return (`Ok (seq,uid,st.Unix.st_size,file))
      ) (fun _ -> return `NotFound)
    in
    (* search through uid/filename list *)
    let find uid uids = 
      List.find (fun (_,u,_) -> u = uid) uids
    in
    match position with
    | `Sequence seq -> 
      if seq > len then
        return `Eof
      else if seq = 0 then
        return `NotFound
      else
        let (_,uid,file) = List.nth uids (seq - 1)
        in (size t seq uid file)
    | `UID uid ->
      try 
        let (seq,uid,file) = find uid uids in (size t seq uid file)
      with _ ->
        let (_,u,_) = List.nth uids (len - 1) in
        if uid > u then
          return `Eof
        else
          return `NotFound
    end

  (* delete a message *)
  let delete_message t position =
    fetch_uidlist t  >>= fun uids ->
    get_file t position uids >>= function
    | `Ok (_,uid,_,file) ->
      let uids = List.filter (fun (_,u,_) -> u <> uid) uids in
      update_uidlist t uids >>= fun () ->
      Lwt_unix.unlink (current t file)
    | _ -> return ()

  let assemble_message t lazy_read metadata =
    let (_,priv) = t.keys in
    let priv = Utils.option_value_exn ~ex:EmptyPrivateKey priv in
    Email_parse.message_from_blob t.config t.keys lazy_read 
    (fun postmark headers content attachment ->
      return (`Ok (
        build_lazy_message_inst
          (module Irmin_core.LazyIrminMessage)
          ((fun () -> postmark ()),
          (fun () -> headers ()),
          (fun () -> content ()),
          (attachment),
          (fun () ->
            return metadata
          ))
        )
      )
    )

  let fetch_ t position uids =
    Printexc.record_backtrace true;
    let skip_postmark ci =
      if t.config.maildir_parse then
        return ()
      else (
        Lwt_io.read_line ci >>= fun _ ->
        return ()
      )
    in
    get_file t position uids >>= function
    | `Ok (_,uid,size,file) ->
      let (internal_date,_,modseq,flags) = message_file_name_to_data t.mailbox file in
      let metadata = {uid;modseq;internal_date;size;flags} in
      let lazy_read = Lazy.from_fun (fun () ->
        let t1 = Unix.gettimeofday () in
        let path = current t file in
        Lwt_io.with_file path ~mode:Lwt_io.Input (fun ci ->
          Lwt_io.read ci
        ) >>= fun msg ->
        let msg = decrypt t msg in
        Stats.add_readt (Unix.gettimeofday() -. t1);
        return msg) in
      if t.config.maildir_parse then (
          assemble_message t lazy_read metadata
      ) else (
        let lazy_message = Lazy.from_fun (fun () ->
        Lazy.force lazy_read >>= fun buffer ->
        let seq = Mailbox.With_seq.of_string buffer in
        return (Utils.option_value_exn 
            (Mailbox.With_seq.fold_message seq ~f:(fun _ message -> Some
            message) ~init:None))) in
        let lazy_metadata = Lazy.from_fun (fun () -> return metadata) in
        return (`Ok (Lazy_message.build_lazy_message_inst (module LazyMaildirMessage)
            (lazy_read, lazy_message, lazy_metadata)))
      )
    | `Eof -> return `Eof
    | `NotFound -> return `NotFound

  let total_fetch = ref 0.
  (* fetch messages from selected mailbox *)
  let fetch t position =
    fetch_uidlist t >>= fun uids ->
    fetch_ t position uids

  (* fetch messages from selected mailbox *)
  let fetch_message_metadata t position =
    fetch_uidlist t >>= fun uids ->
    get_file t position uids >>= function
    | `Ok (_,uid,size,file) ->
      let (internal_date,_,modseq,flags) = message_file_name_to_data t.mailbox file in
      return (`Ok {uid; modseq; internal_date; size; flags})
    | `Eof -> return `Eof
    | `NotFound -> return `NotFound

  (* store flags to selected mailbox *)
  let store t position message_metadata =
    fetch_uidlist t >>= fun uids ->
    get_file t position uids >>= function
    | `Ok (seq,uid,_,src) ->
      let dst = update_message_file_name (MaildirPath.to_maildir t.mailbox) src message_metadata in
      Lwt_unix.rename (current t src) (current t dst) >>= fun () ->
      let uids = List.fold_right (fun (s,u,f) acc -> 
        if u = uid then 
          (s,u,dst) :: acc 
        else 
          (s,u,f) :: acc
      ) uids [] in
      update_uidlist t uids 
    | _ -> return ()

  (* store mailbox metadata *)
  let store_mailbox_metadata t mailbox_metadata =
    update_mailbox_metadata t mailbox_metadata

  (* copy messages from selected mailbox *)
  let copy t pos t2 message_metadata =
    fetch t pos >>= function
    | `Eof | `NotFound -> return ()
    | `Ok (module LazyMessage) ->
      LazyMessage.LazyMessage.get_postmark LazyMessage.this >>= fun postmark ->
      LazyMessage.LazyMessage.get_email LazyMessage.this >>= fun (module LE:LazyEmail_inst) ->
      LE.LazyEmail.to_string LE.this >>= fun email ->
      let message = String.concat Email_parse.crlf [postmark ; email] in
      append t2 message message_metadata

  let commit t =
    return ()

  let uid_to_seq t uid =
    fetch_uidlist t >>= fun uids ->
    get_file t (`UID uid) uids >>= function
    | `Ok (seq,_,_,_) -> return (Some seq)
    | _ -> return None
    
  let create_account t =
    let path = subscribe_path t.config.mail_path t.user in
    Utils.exists path Unix.S_REG >>= fun res ->
    if res then
      return `Exists
    else (
      Lwt_unix.openfile path [Unix.O_WRONLY;Unix.O_CREAT] 0o664 >>= fun fd ->
      Lwt_unix.close fd >>
      write_subscribe path [] >>
      return `Ok
    )
    
  let delete_account t =
    let path = subscribe_path t.config.mail_path t.user in
    Lwt_unix.system ("rm -rf " ^ path) >>= fun _ ->
    return ()
end
