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

type storage_ = {store:Gitl.t ref; user: string; mailbox: Key.t; config: imapConfig; keys: 
  Ssl_.keys; index: (string * int) list ref; gitl_cache: cache_type;root_tree:Sha.t ref}

module GitlStorage : Storage_intf with type t = storage_ = 
struct
  type t = storage_

  (* user,mailbox,keys *)
  let create config user mailbox keys =
    let cache = ref MapStr.empty in
    Gitl.create ~cache ~compress:config.compress_repo ~repo:config.irmin_path () >>= fun store ->
    return {store=ref store;user;mailbox=Key.of_unix mailbox;config;keys;
    index=ref [];gitl_cache=cache;root_tree=ref Sha.empty}

  let update_with_sha t ?(root=false) k v =
    let key = if root then [k] else Key.add t.mailbox k in
    Gitl.update t.!store key v >>= fun (v'sha,sha) ->
    t.root_tree := sha;
    return (v'sha,sha)

  let update t ?(root=false) k v =
    update_with_sha t ~root k v >>= fun (_,_) ->
    return ()

  let remove ?key t =
    let key = match key with
    | None -> t.mailbox
    | Some k -> Key.add t.mailbox k
    in
    Gitl.remove t.!store key >>= fun sha ->
    t.root_tree := sha;
    return ()

  (* read message's index *)
  let read_index t =
    if t.!index = [] then (
      Gitl.read_exn t.!store (Key.add t.mailbox ".index") >>= fun sexp_str ->
      let uids = list_of_sexp
        (fun i ->
          pair_of_sexp (fun a -> Sexp.to_string a) (fun b -> int_of_string (Sexp.to_string b)) i
        ) (Sexp.of_string sexp_str) in
      t.index := uids;
      return uids
    ) else (
      return t.!index
    )

  (* update message's index *)
  let update_index t uids =
    t.index := uids;
    update t ".index"
      (Sexp.to_string (
        sexp_of_list (
          fun i -> sexp_of_pair 
            (fun a -> Sexp.of_string a) (fun b -> Sexp.of_string (string_of_int b)) i
          ) uids
      )) >>= fun _ ->
    return ()

  (* check if mailbox exists *)
  let exists t =
    Gitl.mem t.!store t.mailbox >>= function
    | true -> return `Mailbox
    | false -> return `No

  (* status *)
  let status t =
    Gitl.read_exn t.!store (Key.add t.mailbox ".meta") >>= fun sexp_str ->
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
    update t ".meta" (Sexp.to_string sexp) >>= fun _ ->
    update_index t []

  (* delete mailbox *)
  let delete t =
    remove ~key:".meta" t >>
    remove ~key:".index" t >>
    remove t

  (* rename mailbox1 mailbox2 *)
  let rename t mailbox =
    Gitl.rename t.!store t.mailbox (Key.of_unix mailbox) >>= fun sha ->
    t.root_tree := sha;
    return ()

  let read_subscription t =
    Gitl.read_opt t.!store [".subscription"] >>= function
    | Some sexp_str -> return (Utils.list_of_str_sexp sexp_str)
    | None -> return []

  (* subscribe mailbox *)
  let subscribe t =
    read_subscription t >>= fun l ->
    let mailbox = Key.to_string t.mailbox in
    update t ~root:true ".subscription" (Utils.str_sexp_of_list (
      mailbox :: (List.filter (fun e -> e <> mailbox) l)))

  (* unsubscribe mailbox *)
  let unsubscribe t =
    read_subscription t >>= fun l ->
    let mailbox = Key.to_string t.mailbox in
    update t ~root:true ".subscription" (Utils.str_sexp_of_list (
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
      Gitl.list t.!store key >>= fun listing ->
      Lwt_list.fold_left_s (fun (acc,cnt) name ->
        if access (Key.to_string key) = false then
          return (acc,cnt)
        else (
          list_ name acc >>= fun (acc,cnt) ->
          f acc (`Mailbox ((Key.to_string name),cnt)) >>= fun acc ->
          return (acc,cnt+1)
        )
      ) (init,0) listing
    in
    list_ t.mailbox init >>= fun (acc,_) ->
    return acc

  (* append message(s) to selected mailbox *)
  let append t message message_metadata =
    let (pub_key,_) = t.keys in
    Email_parse.do_encrypt pub_key t.config message >>= fun message ->
    update_with_sha t "*" message >>= fun (v'sha,_) ->
    let sha_str = Sha.to_string v'sha in
    update t (".meta." ^ sha_str)
      (Sexp.to_string (sexp_of_mailbox_message_metadata message_metadata)) >>
    read_index t >>= fun index ->
    update_index t ((sha_str,message_metadata.uid) :: index)

  let get_uid t pos =
    read_index t >>= fun uids ->
    match pos with
    | `Sequence seq ->
      if seq > List.length uids then
        return `Eof
      else if seq = 0 then
        return `NotFound
      else (
        let (sha,uid) = List.nth uids ((List.length uids) - seq) in
        return (`Ok (seq,uid,sha))
      )
    | `UID uid ->
      (* should do binary search, the list is ordered *)
      match Utils.list_findi uids (fun i (_,u) -> u = uid) with
      | None -> 
        let nth_uid uids n = 
          let (_,uid) = List.nth uids n in
          uid
        in
        if uid > nth_uid uids 0 then
          return `Eof
        else
          return `NotFound
      | Some (seq,(sha,uid)) ->
        return (`Ok ((List.length uids) - seq,uid,sha))

  let delete_ t k =
    Gitl.remove t.!store (Key.add t.mailbox k) >>= fun sha ->
    t.root_tree := sha;
    return ()

  (* delete a message
   *)
  let delete_message t pos =
    get_uid t pos >>= function
    | `Ok (_,uid,sha) ->
      delete_ t sha >>
      delete_ t (".meta." ^ sha) >>
      read_index t >>= fun uids ->
      update_index t (List.filter (fun (_,u) -> u <> uid) uids)
    | _ -> return ()

  let get_message_metadata t sha =
    Gitl.read_exn t.!store (Key.add t.mailbox (".meta." ^ sha)) >>= fun sexp_str ->
    return (mailbox_message_metadata_of_sexp (Sexp.of_string sexp_str))

  (* fetch messages from selected mailbox
  *)
  let fetch t pos =
    get_uid t pos >>= function
    | `Ok (_,_,sha) -> 
      let open Lazy_maildir_message in
      let open Parsemail in
      let lazy_read = Lazy.from_fun (fun () -> 
        let tm = Unix.gettimeofday () in
        Gitl.read_exn t.!store (Key.add t.mailbox sha)  >>= fun message ->
        Email_parse.message_unparsed_from_blob t.config t.keys message >>= fun message ->
        Stats.add_readt (Unix.gettimeofday() -. tm);
        return message
      ) in
      let lazy_message = Lazy.from_fun (fun () ->
        Lazy.force lazy_read >>= fun buffer ->
        let seq = Mailbox.With_seq.of_string buffer in
        return (Utils.option_value_exn (Mailbox.With_seq.fold_message seq
          ~f:(fun _ message -> Some message) ~init:None))) in
      let lazy_metadata = Lazy.from_fun (fun () -> 
        let tm = Unix.gettimeofday() in
        get_message_metadata t sha >>= fun meta ->
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
    | `Ok (_,_,sha) -> 
      get_message_metadata t sha >>= fun meta ->
      return (`Ok meta)
    | `Eof -> return `Eof
    | `NotFound -> return `NotFound

  (* store flags to selected mailbox *)
  let store t pos message_metadata =
    get_uid t pos >>= function
    | `Ok (_,_,sha) -> 
      update t (".meta." ^ sha)
        (Sexp.to_string (sexp_of_mailbox_message_metadata message_metadata))
    | _ -> return ()

  (* store mailbox metadata *)
  let store_mailbox_metadata t metadata =
    update t ".meta" (Sexp.to_string (sexp_of_mailbox_metadata metadata))

  (* copy message from the source mailbox at the given position 
   * to the destination mailbox
   *)
  let copy t pos t2 message_metadata =
    get_uid t pos >>= function
    | `Ok (_,_,sha) -> (* message's sha *)
      Gitl.find_sha_exn t.!store (Key.add t.mailbox (".meta." ^ sha)) >>= fun meta_sha ->
      Gitl.update_tree t.!store t2.mailbox (`Add (".meta",`Normal,meta_sha)) >>= fun _ -> 
      Gitl.update_tree t.!store t2.mailbox (`Add (sha,`Normal,Sha.of_hex_string sha)) >>= fun sha -> 
      t.root_tree := sha;
      return ()
    | _ -> return ()

  (* all operations that update the mailbox have to be completed with commit
   *)
  let commit t =
    Gitl.commit t.!store t.!root_tree ~author:"<user@gitl>" 
      ~message:"consolidated commit" >>= fun store ->
    t.store := store;
    return ()

  (* get sequence # for the given uid *)
  let uid_to_seq t uid =
    get_uid t (`UID uid) >>= function
    | `Ok (seq,_,_) -> return (Some seq)
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
