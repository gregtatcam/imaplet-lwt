(*
 * Copyright (c) 2013-2016 Gregory Tsipenyuk <gregtsip@cam.ac.uk>
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
open Server_config
open Mail_file_name
open Gitl

(* store message in git like storage, the only difference so far is in having
 * compression disabled, which is not the same as Zlib.deflate level=0 
 * mailbox structured as tree hierarchy with mailbox being able to store both
 * messages and other mailboxes. Mailbox metadata is stored under ".meta"^hash  leaf. 
 * Message is stored under message's hash leaf
 * (hash), message metadata is stored under ".msg_meta/hash" leaf. Mailbox index is
 * stored under ".index" leaf.
 * For instance mailbox /foo/foo1 has this structure
 * /foo/foo1/.index
 * /foo/foo1/.meta
 * /foo/foo1/.msg_meta/hash1
 * /foo/foo1/.msg_meta/hash2
 * /foo/foo1/hash1
 * /foo/foo1/hash2
 * Path is converted into Gitl key as /foo/foo1/.index = ["foo";"foo1";".index"]
 *)

module MapStr = Map.Make(String)

type storage_ = {store:Gitl.t ref; user: string; mailbox: Key.t; config: imapConfig; keys: Ssl_.keys}

module GitlStorage : Storage_intf with type t = storage_ = 
struct
  type t = storage_

  let metamessage_dir = "/.metamessage/"
  let message_dir = "/.message/"

  let get_key t = function
    | `Index -> Key.add t.mailbox ".index"
    | `Metamessage (uid,id) -> 
      Key.add t.mailbox (String.concat "" [metamessage_dir; string_of_int uid; "."; id])
    | `Metamailbox -> Key.add t.mailbox ".metamailbox"
    | `Message id -> Key.add t.mailbox (message_dir ^ id)
    | `Subscription -> Key.of_unix ".subscription"

  (* user,mailbox,keys *)
  let create config user mailbox keys =
    Gitl.create ?compress:config.compress_repo ~repo:config.irmin_path () >>= fun store ->
    return {store=ref store;user;mailbox=Key.of_unix mailbox;config;keys}

  let update_with_sha t k v =
    Gitl.update !(t.store) k v

  let update t k v =
    update_with_sha t k v >>= fun (_,_) ->
    return ()

  let remove ?key t =
    let key = match key with
    | None -> t.mailbox
    | Some k -> Key.add t.mailbox k
    in
    Gitl.remove !(t.store) key >>= fun sha ->
    return ()

  (* check if mailbox exists *)
  let exists t =
    Gitl.mem !(t.store) t.mailbox >>= function
    | true -> return `Mailbox
    | false -> return `No

  (* status *)
  let status t =
    Gitl.read_exn !(t.store) (get_key t `Metamailbox) >>= fun sexp_str ->
    let sexp = Sexp.of_string sexp_str in
    return (mailbox_metadata_of_sexp sexp)

  (* select mailbox *)
  let select t =
    status t

  (* examine mailbox *)
  let examine t =
    status t

  (* create mailbox *)
  let create_mailbox t =
    let metadata = empty_mailbox_metadata ~uidvalidity:(new_uidvalidity())()
      ~selectable:true in
    let sexp = sexp_of_mailbox_metadata metadata in
    update t (get_key t `Metamailbox) (Sexp.to_string sexp) >>= fun _ ->
    return ()

  (* delete mailbox *)
  let delete t =
    remove t

  (* rename mailbox1 mailbox2 *)
  let rename t mailbox =
    Gitl.rename !(t.store) t.mailbox (Key.of_unix mailbox) >>= fun sha ->
    return ()

  let read_subscription t =
    Gitl.read_opt !(t.store) (get_key t `Subscription) >>= function
    | Some sexp_str -> return (Utils.list_of_str_sexp sexp_str)
    | None -> return []

  (* subscribe mailbox *)
  let subscribe t =
    read_subscription t >>= fun l ->
    let mailbox = Key.to_string t.mailbox in
    update t (get_key t `Subscription) (Utils.str_sexp_of_list (
      mailbox :: (List.filter (fun e -> e <> mailbox) l)))

  (* unsubscribe mailbox *)
  let unsubscribe t =
    read_subscription t >>= fun l ->
    let mailbox = Key.to_string t.mailbox in
    update t (get_key t `Subscription) (Utils.str_sexp_of_list (
      (List.filter (fun e -> e <> mailbox) l)))

  (* list reference mailbox 
   * returns list of files/folders with list of flags 
  let list : t -> subscribed:bool -> ?access:(string->bool) -> init:'a -> 
    f:('a -> [`Folder of (string*int)|`Mailbox of (string*int)] -> 'a Lwt.t) -> 'a Lwt.t
   *)
  let list t ~subscribed ?(access=(fun _ -> true)) ~init ~f =
    (* need to eliminate messages, .index, and .meta *)
    begin
    if subscribed then 
      read_subscription t
    else
      return []
    end >>= fun subs ->
    let rec list_ key init =
      Gitl.list !(t.store) key >>= fun listing ->
      Lwt_list.fold_left_s (fun (acc,cnt) name ->
        let leaf = Key.last name in
        if access (Key.to_string name) = false || leaf = ".message" || 
            leaf = ".index" || leaf = ".metamessage" || leaf = ".metamailbox" 
            || leaf = ".subscription" then
          return (acc,cnt)
        else (
          list_ name acc >>= fun (acc,cnt) ->
          f acc (`Mailbox ((Key.to_string ~absolute_path:false name),cnt)) >>= fun acc ->
          return (acc,cnt+1)
        )
      ) (init,0) listing
    in
    list_ t.mailbox init >>= fun (acc,_) ->
    return acc

  (* append message(s) to selected mailbox *)
  let append t message message_metadata =
    let (pub_key,_) = t.keys in
    begin
      if t.config.maildir_parse then
        Lightparsemail.Message.to_parsed_message_with_header message
      else
        return message
    end >>= fun message ->
    Email_parse.do_encrypt pub_key t.config message >>= fun message ->
    if t.config.hybrid then (
      let obj = Object.create ?compress:t.config.compress_repo t.config.irmin_path in
      Object.write_raw obj message >>= fun v'sha ->
      let sha_str = Sha.to_string v'sha in
      update t (get_key t (`Metamessage (message_metadata.uid, sha_str)))
        (Sexp.to_string (sexp_of_mailbox_message_metadata message_metadata))
    ) else (
      let id = Printf.sprintf "%d.<*>" message_metadata.uid in
      let file = make_message_file_name ~init_file:(init_message_file_name_id id)
        t.mailbox message_metadata in
      update t (get_key t (`Message file)) message 
    )

  let re_uid_sha = Re_posix.compile_pat "^([0-9]+)\\.([^\\.]+)"

  let get_uid t pos =
    let uid_sha str =
      let subs = Re.exec re_uid_sha str in
      (int_of_string (Re.get subs 1),Re.get subs 2)
    in
    begin
    if t.config.hybrid then
      Gitl.find_obj_opt !(t.store) (Key.add t.mailbox metamessage_dir)
    else
      Gitl.find_obj_opt !(t.store) (Key.add t.mailbox message_dir)
    end >>= function
    | `Tree tr ->
      let len = List.length tr in
      begin
      match pos with
      | `Sequence seq ->
        if seq > len then
          return `Eof
        else if seq = 0 then
          return `NotFound
        else (
          let te = List.nth tr (len - seq) in
          let uid,sha = uid_sha !te.name in
          Log_.log `Debug (Printf.sprintf "get_uid by seq: seq: %d, uid: %d, %s\n" 
            seq uid !te.name);
          return (`Ok (seq,uid,sha,!te.name))
        )
      | `UID uid ->
        (* should do binary search, the list is ordered *)
        match Utils.list_findi tr (fun i te -> 
          Re.execp (Re_posix.compile_pat ("^" ^ (string_of_int uid) ^ "\\.")) !te.name
        ) with
        | None -> 
          let nth_uid tr n = 
            let te = List.nth tr n in
            let (uid,_) = uid_sha !te.name in
            uid
          in
          if uid > nth_uid tr 0 then
            return `Eof
          else
            return `NotFound
        | Some (seq,te) ->
          let uid,sha = uid_sha !te.name in
          Log_.log `Debug (Printf.sprintf "get_uid by uid: seq: %d, uid: %d, %s\n" 
            (len-seq) uid !te.name);
          return (`Ok (len-seq,uid,sha,!te.name))
      end
    | _ -> return `NotFound

  let delete_ t k =
    Gitl.remove !(t.store) k >>= fun sha ->
    return ()

  (* delete a message
   *)
  let delete_message t pos =
    get_uid t pos >>= function
    | `Ok (_,uid,sha,name) ->
      if t.config.hybrid then
        delete_ t (get_key t (`Metamessage (uid, sha)))
      else
        delete_ t (get_key t (`Message name))
    | _ -> return ()

  let get_message_metadata t uid sha name =
    if t.config.hybrid then
      Gitl.read_exn !(t.store) (get_key t (`Metamessage (uid, sha))) >>= fun sexp_str ->
      return (mailbox_message_metadata_of_sexp (Sexp.of_string sexp_str))
    else (
      let (internal_date,size,modseq,flags) = message_file_name_to_data t.mailbox name in
      return {uid;modseq;internal_date;size;flags}
    )

  (* fetch messages from selected mailbox
  *)
  let fetch t pos =
    get_uid t pos >>= function
    | `Ok (_,uid,sha,name) -> 
      let open Lazy_maildir_message in
      let open Parsemail in
      let lazy_read = Lazy.from_fun (fun () -> 
        let tm = Unix.gettimeofday () in
        begin
        if t.config.hybrid then (
          let obj = Object.create ?compress:t.config.compress_repo t.config.irmin_path in
          Object.read_raw obj (Sha.of_hex_string sha)
        ) else (
          Gitl.read_exn !(t.store) (get_key t (`Message name))
        )
        end >>= fun message ->
        Email_parse.message_unparsed_from_blob t.config t.keys message >>= fun message ->
        Stats.add_readt (Unix.gettimeofday() -. tm);
        return message
      ) in
      let lazy_message = Lazy.from_fun (fun () ->
        Lazy.force lazy_read >>= fun buffer ->
          if t.config.maildir_parse then
            return (Lightparsemail.Message.from_parsed_message_with_header buffer)
          else
            Lightparsemail.Message.parse buffer
        ) in
      let lazy_metadata = Lazy.from_fun (fun () -> 
        let tm = Unix.gettimeofday() in
        get_message_metadata t uid sha name >>= fun meta ->
        Stats.add_readmetat (Unix.gettimeofday() -. tm);
        return meta) in
      return (`Ok  (Lazy_message.build_lazy_message_inst (module LazyMaildirMessage) 
        (lazy_read, lazy_message, lazy_metadata)))
    | `Eof -> return `Eof
    | `NotFound -> return `NotFound

  (* fetch messages from selected mailbox
  *)
  let fetch_message_metadata t pos =
    get_uid t pos >>= function
    | `Ok (_,uid,sha,name) -> 
      get_message_metadata t uid sha name >>= fun meta ->
      return (`Ok meta)
    | `Eof -> return `Eof
    | `NotFound -> return `NotFound

  (* store flags to selected mailbox *)
  let store t pos message_metadata =
    get_uid t pos >>= function
    | `Ok (_,uid,sha,src) -> 
      if t.config.hybrid then
        update t (get_key t (`Metamessage (uid, sha)))
          (Sexp.to_string (sexp_of_mailbox_message_metadata message_metadata))
      else (
        let dest = update_message_file_name t.mailbox src message_metadata in
        let src = get_key t (`Message src) in
        let dest = get_key t (`Message dest) in
        Log_.log `Debug (Printf.sprintf "renaming %s to %s\n" (Key.to_string src) (Key.to_string dest));
        Gitl.rename !(t.store) ~src  ~dest >>= fun _ ->
        return ()
      )
    | _ -> return ()

  (* store mailbox metadata *)
  let store_mailbox_metadata t metadata =
    update t (get_key t `Metamailbox) (Sexp.to_string (sexp_of_mailbox_metadata metadata))

  (* copy message from the source mailbox at the given position 
   * to the destination mailbox
   *)
  let copy t pos t2 message_metadata =
    get_uid t pos >>= function
    | `Ok (_,uid,sha,name) -> (* message's sha *)
      if t.config.hybrid then (
        update t2 (get_key t2 (`Metamessage (message_metadata.uid, sha)))
          (Sexp.to_string (sexp_of_mailbox_message_metadata message_metadata))
      ) else (
        let file = update_message_file_name t2.mailbox name message_metadata in
        Gitl.update_tree !(t.store) (Key.add t2.mailbox message_dir) 
          (`Add (file,`Normal,Sha.of_hex_string sha)) >>= fun _ ->
        return ()
      )
    | _ -> return ()

  (* all operations that update the mailbox have to be completed with commit
   *)
  let commit t =
    Gitl.commit !(t.store) ~author:"<user@gitl>" 
      ~message:"consolidated commit" >>= fun store ->
    t.store := store;
    return ()

  (* get sequence # for the given uid *)
  let uid_to_seq t uid =
    get_uid t (`UID uid) >>= function
    | `Ok (seq,_,_,_) -> return (Some seq)
    | _ -> return None

  (* create user account *)
  let create_account t =
    (* don't need to do anything??? *)
    return `Ok

  (* delete user account *)
  let delete_account t =
    (* don't delete anything??? *)
    return ()
end
