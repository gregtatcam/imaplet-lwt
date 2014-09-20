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
open Irmin_unix
open Core.Std
open BatLog
open Sexplib
open Email_message
open Email_message.Mailbox.Message
open Server_config
open Imaplet_types
open Storage_meta

exception KeyDoesntExist
exception DuplicateUID

let path = srv_config.irmin_path

module Git = IrminGit.FS(struct
  let root = Some path
  let bare = true
end)

module Store = Git.Make(IrminKey.SHA1)(IrminContents.String)(IrminTag.String)

module IrminKey :
  sig
    type t

    (* user and unix path to key *)
    val create : string -> t
    val create_account : string -> t
    val create_mailboxes : string -> string -> (string*bool*t)
    val t_to_list : t -> string list
    val t_to_irmin_key : t -> Store.key
    val key_to_string : t -> string
    val key_to_path : t -> string
  end =
  struct
    type t = Store.key

    let create_account user =
      ["imaplet";user]

    let create str =
      let path = String.lstrip ~drop:(fun c -> c = '/') path in
      let path = String.rstrip ~drop:(fun c -> c = '/') path in
      String.split path ~on:'/'

    let create_mailboxes user path =
      let key = String.split path ~on:'/' in
      let mailbox,folder,key = List.fold_right key ~init:("",false,[]) 
      ~f:(fun i (mailbox,folder,acc) -> 
        if i <> "" then (
          if List.length acc <> 0 then
            mailbox,folder,"mailboxes" :: (i :: acc)
          else
            i,folder,"mailboxes" :: (i :: acc)
        ) else if List.length acc = 0 then
          mailbox,true,acc
        else
          mailbox,folder,acc
      ) in
      mailbox,folder,"imaplet" :: (user :: key)

    let t_to_list key = key

    let t_to_irmin_key key = key

    (* convert key to path, keep "imaplet", user, and "mailboxes" *)
    let key_to_string key = 
      List.fold key 
      ~init:""
      ~f:(fun acc item -> 
        if acc = "" then
          "/" ^ item
        else
          acc ^ "/" ^ item
     ) 

    (* convert key to path, remove "imaplet",user, and "mailboxes" *)
    let key_to_path key = 
     List.foldi key 
     ~f:(fun i acc item -> 
       if i < 2 then (* skip imaplet and user *)
         acc
       else if i % 2 = 0 then ( (* skip mailboxes *) 
         if acc = "" then
           acc
         else
           acc ^ "/" 
       ) else 
         acc ^ item
       ) ~init:""

    (* convert view key (relative key) to path, remove "mailboxes" *)
    let view_key_to_path key = 
     List.foldi key 
     ~f:(fun i acc item -> 
       if i % 2 = 0 then ( (* skip mailboxes *) 
         if acc = "" then
           acc
         else
           acc ^ "/" 
       ) else 
         acc ^ item
       ) ~init:""

  end

module IrminIntf :
  sig
    type store
    type transaction
    val create : unit -> store Lwt.t
    val remove : store -> IrminKey.t -> unit Lwt.t
    val update_view : store -> IrminKey.t -> Store.View.t -> unit Lwt.t
    val read_view : store -> IrminKey.t -> Store.View.t Lwt.t
    val remove_view : transaction -> unit Lwt.t
    val move_view : transaction -> IrminKey.t -> unit Lwt.t
    val begin_transaction : IrminKey.t -> transaction Lwt.t
    val end_transaction : transaction -> unit Lwt.t
    val tr_update : transaction -> IrminKey.t -> string -> unit Lwt.t
    val tr_read : transaction -> IrminKey.t -> string option Lwt.t
    val tr_read_exn : transaction -> IrminKey.t -> string Lwt.t
    val tr_list : transaction -> IrminKey.t -> Store.key list Lwt.t
    val tr_remove : transaction -> IrminKey.t -> unit Lwt.t
    val tr_mem : transaction -> IrminKey.t -> bool Lwt.t
  end =
  struct
    type store = Store.t
    type transaction = Store.t * Store.View.t * IrminKey.t * bool ref


    let create () =
      Store.create ()

    let remove store key =
      Store.remove store (IrminKey.t_to_irmin_key key)

    let update_view store key view =
      Easy.logf `debug "------ store update_view %s\n%!" (IrminKey.key_to_string key);
      Store.View.update_path store (IrminKey.t_to_irmin_key key) view

    let read_view store key =
      Easy.logf `debug "------ reading view %s\n%!" (IrminKey.key_to_string key);
      Store.View.of_path store (IrminKey.t_to_irmin_key key)

    let remove_view tr =
      let (store,_,key,_) = tr in
      remove store key

    let begin_transaction key =
      Store.create () >>= fun store ->
      Easy.logf `debug "------ creating view %s\n%!" (IrminKey.key_to_string key);
      Store.View.of_path store (IrminKey.t_to_irmin_key key) >>= fun view ->
      return (store,view,key,ref false)

    let move_view tr key2 =
      let (store,view,_,_) = tr in
      Store.View.update_path store (IrminKey.t_to_irmin_key key2) view

    let end_transaction tr =
      let (store,view,key,dirty) = tr in
      if !dirty = true then (
        Easy.logf `debug "++++++++++++++++++ commiting!!!\n%!";
        Store.View.update_path store (IrminKey.t_to_irmin_key key) view >>= fun () ->
        dirty := false;
        return ()
      ) else
        return ()

    let tr_update tr key data =
      Easy.logf `debug "------ store view.update %s\n" (IrminKey.key_to_string key);
      let (_,view,_,dirty) = tr in
      Store.View.update view (IrminKey.t_to_irmin_key key) data >>= fun () ->
      dirty := true;
      return ()

    let tr_read tr key =
      let (_,view,_,_) = tr in
      Store.View.read view (IrminKey.t_to_irmin_key key)

    let tr_read_exn tr key =
      let (_,view,_,_) = tr in
      Store.View.read_exn view (IrminKey.t_to_irmin_key key)

    let tr_list tr key =
      Easy.logf `debug "------ store list %s\n%!" (IrminKey.key_to_string key);
      let (_,view,_,_) = tr in
      Store.View.list view [IrminKey.t_to_irmin_key key]

    let tr_remove tr key =
      Easy.logf `debug "------ store remove %s\n" (IrminKey.key_to_string key);
      let (_,view,_,dirty) = tr in
      Store.View.remove view (IrminKey.t_to_irmin_key key) >>= fun () ->
      dirty := true;
      return ()

    let tr_mem tr key =
      let (_,view,_,_) = tr in
      Store.View.mem view (IrminKey.t_to_irmin_key key)

  end

(* mailboxes subscription *)
module Subscriptions :
  sig
    type t
    val key_subscr : IrminKey.t
    val create : string -> t Lwt.t
    val read : t -> string list Lwt.t
    val subscribe : t -> string -> unit Lwt.t
    val unsubscribe : t -> string -> unit Lwt.t
    val empty : string
  end =
  struct
    type t = IrminIntf.transaction

    let key_subscr = IrminKey.create "subscriptions"

    (* create type *)
    let create user =
      let key = IrminKey.create_account user in
      IrminIntf.begin_transaction key

    (* convert the list to a string of sexp *)
    let str_sexp_of_list l =
      let sexp = List.sexp_of_t (fun i -> Sexp.of_string i) l in
      Sexp.to_string sexp

    let empty =
      str_sexp_of_list []

    (* convert string of sexp to the list *)
    let list_of_str_sexp str =
      let sexp = Sexp.of_string str in
      (List.t_of_sexp (fun i -> Sexp.to_string i) sexp)

    (* update subscription list *)
    let update_exn view l =
      IrminIntf.tr_mem view key_subscr >>= fun res ->
      if res = false then raise KeyDoesntExist;
      let str = str_sexp_of_list l in
      IrminIntf.tr_update view key_subscr str >>
      IrminIntf.end_transaction view

    (* read subscription *)
    let read view =
      IrminIntf.tr_read view key_subscr >>= function
      | Some str -> return (list_of_str_sexp str)
      | None -> return []

    (* subscribe *)
    let subscribe t mailbox =
      read t >>= fun l ->
      if (List.find l ~f:(fun i -> if i = mailbox then true else false) <> None) then 
        return ()
      else
        update_exn t (mailbox :: l) 

    (* unsubscribe *)
    let unsubscribe t mailbox =
      read t >>= fun l ->
      let l = List.fold l ~init:[] ~f:(fun acc i -> if i = mailbox then acc else i :: acc) in
      update_exn t l
  end

module UserAccount :
  sig
    type t

    val create : string -> t
    val create_account : t -> [`Exists|`Ok] Lwt.t
    val delete_account : t -> unit Lwt.t
  end = 
  struct
    type t = IrminKey.t

    (* create type *)
    let create user = 
      IrminKey.create_account user

    (* create new account *)
    let create_account key =
      IrminIntf.begin_transaction key >>= fun view ->
      IrminIntf.tr_mem view Subscriptions.key_subscr >>= fun res ->
      if res then
        return `Exists
      else (
        IrminIntf.tr_update view Subscriptions.key_subscr Subscriptions.empty >>
        IrminIntf.end_transaction view >>
        return `Ok
      )

    (* remove account *)
    let delete_account key =
      IrminIntf.begin_transaction key >>= fun view ->
      IrminIntf.tr_remove view Subscriptions.key_subscr >>
      IrminIntf.end_transaction view

  end

(* consistency TBD *)
module IrminMailbox :
  sig
    type t
    val create : string -> string  -> t Lwt.t
    val commit : t -> unit Lwt.t
    val exists : t -> [`No|`Folder|`Mailbox] Lwt.t
    val create_mailbox : t -> unit Lwt.t
    val delete_mailbox : t -> unit Lwt.t
    val move_mailbox : t -> string -> unit Lwt.t
    val copy_mailbox : t -> t -> sequence -> bool -> unit Lwt.t
    val read_mailbox_metadata : t -> mailbox_metadata Lwt.t
    val append_message : t -> Mailbox.Message.t -> mailbox_message_metadata -> unit Lwt.t
    val update_mailbox_metadata : t -> mailbox_metadata -> unit Lwt.t
    val update_message_metadata : t -> [`Sequence of int|`UID of int] -> mailbox_message_metadata ->
      [`NotFound|`Eof|`Ok] Lwt.t
    val read_message : t -> ?filter:(searchKey) searchKeys -> 
      [`Sequence of int|`UID of int] ->
      [`NotFound|`Eof|`Ok of (Mailbox.Message.t * mailbox_message_metadata)] Lwt.t
    val read_message_metadata : t -> [`Sequence of int|`UID of int] -> 
      [`NotFound|`Eof|`Ok of mailbox_message_metadata] Lwt.t
    val delete_message : t -> [`Sequence of int|`UID of int] -> unit Lwt.t
    val list : t -> [`Folder of string|`Mailbox of string] list Lwt.t
  end =
  struct
    (* user * mailbox * is-folder * irmin key including the mailbox *)
    type t =
      {user:string;mailbox:string;folders:bool;trans:IrminIntf.transaction;
      index:int list option ref}

    let get_key = function
    | `Metamailbox -> IrminKey.create "meta"
    | `Index -> IrminKey.create "index"
    | `Messages -> IrminKey.create "messages"
    | `Postmark uid -> IrminKey.create ("messages/" ^ (string_of_int uid) ^ "/postmark")
    | `Headers uid -> IrminKey.create ("messages/" ^ (string_of_int uid) ^ "/headers")
    | `Email uid -> IrminKey.create ("messages/" ^ (string_of_int uid) ^ "/email")
    | `Metamessage uid -> IrminKey.create ("messages/" ^ (string_of_int uid) ^ "/meta")
    | `Uid uid -> IrminKey.create ("messages/" ^ (string_of_int uid))

    (* commit should be called explicitly on each created mailbox to have the
     * changes commited to Irmin
     *)
    let create user path =
      let (mailbox,folders,key) = IrminKey.create_mailboxes user path in
      IrminIntf.begin_transaction key >>= fun trans ->
        return {user;mailbox;folders;trans;index=ref None}

    let commit mbox =
      IrminIntf.end_transaction mbox.trans

    (* create mailbox metadata and index stores *)
    let create_mailbox mbox =
      let key = get_key `Metamailbox in
      let metadata = empty_mailbox_metadata ~uidvalidity:(new_uidvalidity())()
        ~folders:mbox.folders in
      let sexp = sexp_of_mailbox_metadata metadata in
      IrminIntf.tr_update mbox.trans key (Sexp.to_string sexp) >>= fun () ->
      let key = get_key `Index in
      let sexp = List.sexp_of_t (fun i -> Sexp.of_string i) [] in
      IrminIntf.tr_update mbox.trans key (Sexp.to_string sexp) 

    let delete_mailbox mbox =
      IrminIntf.remove_view mbox.trans

    (* how to make this the transaction? TBD *)
    let move_mailbox mbox path =
      let (_,_,key2) = IrminKey.create_mailboxes mbox.user path in
      IrminIntf.move_view mbox.trans key2 >>
      IrminIntf.remove_view mbox.trans

    let read_mailbox_metadata mbox =
      IrminIntf.tr_read_exn mbox.trans (get_key `Metamailbox) >>= fun sexp_str ->
      let sexp = Sexp.of_string sexp_str in
      return (mailbox_metadata_of_sexp sexp)

    let update_mailbox_metadata mbox metadata =
      let sexp = sexp_of_mailbox_metadata metadata in
      let key = get_key `Metamailbox in
      IrminIntf.tr_update mbox.trans key (Sexp.to_string sexp)

    let exists mbox =
      catch (fun () ->
        read_mailbox_metadata mbox >>= fun metadata ->
        return (if metadata.folders then `Folder else `Mailbox)
      )
      (fun _ -> return `No)

    let find_flag l fl =
      List.find l ~f:(fun f -> if f = fl then true else false) <> None

    let read_index_uid mbox =
      match mbox.!index with
      | None ->
        IrminIntf.tr_read_exn mbox.trans (get_key `Index) >>= fun index_sexp_str ->
        return (List.t_of_sexp (fun i -> int_of_string (Sexp.to_string i)) 
            (Sexp.of_string index_sexp_str))
      | Some uids -> return uids

    let update_index_uid mbox uid =
      read_index_uid mbox >>= fun uids ->
      if List.find uids ~f:(fun u -> if u = uid then true else false) <> None then
        raise DuplicateUID
      else (
        (* reverse index *)
        let uids = (uid :: uids) in
        mbox.index := Some uids;
        IrminIntf.tr_update mbox.trans (get_key `Index) 
          (Sexp.to_string (List.sexp_of_t (fun i -> Sexp.of_string (string_of_int i)) (uids)))
      )

    let append_message mbox message message_metadata =
      read_mailbox_metadata mbox >>= fun mailbox_metadata ->
      let count = mailbox_metadata.count + 1 in
      let uid = mailbox_metadata.uidnext in
      let uidnext = uid + 1 in
      let modseq = Int64.(+) mailbox_metadata.modseq Int64.one in 
      let seen = find_flag message_metadata.flags Flags_Seen in
      let recent = find_flag message_metadata.flags Flags_Recent in
      let nunseen = 
        if seen = false then 
          mailbox_metadata.nunseen + 1 
        else
          mailbox_metadata.nunseen 
      in
      let recent = 
        if recent = true then 
          mailbox_metadata.recent + 1 
        else
          mailbox_metadata.recent 
      in
      let unseen = 
        if mailbox_metadata.unseen = 0 && seen = false then 
          count
        else 
          mailbox_metadata.unseen 
      in
      let mailbox_metadata = {mailbox_metadata with
        uidnext;count;nunseen;recent;unseen} in
      let size = String.length (Mailbox.Message.to_string message) in
      let message_metadata = {message_metadata with uid;modseq;size} in
      update_mailbox_metadata mbox mailbox_metadata >>= fun () ->
      let message_metadata_sexp = sexp_of_mailbox_message_metadata message_metadata in
      IrminIntf.tr_update mbox.trans (get_key (`Metamessage uid)) 
        (Sexp.to_string message_metadata_sexp) >>= fun () ->
      let postmark = message.postmark in
      let postmark_sexp = Mailbox.Postmark.sexp_of_t postmark in
      IrminIntf.tr_update mbox.trans (get_key (`Postmark uid)) 
        (Sexp.to_string postmark_sexp) >>= fun () ->
      let email = message.email in
      let email_sexp = Email.sexp_of_t email in
      IrminIntf.tr_update mbox.trans (get_key (`Email uid)) (Sexp.to_string email_sexp) >>= fun () ->
      let header = Email.header email in
      let header_sexp = Header.sexp_of_t header in
      IrminIntf.tr_update mbox.trans (get_key (`Headers uid)) (Sexp.to_string header_sexp) >>= fun () ->
      update_index_uid mbox uid

    let get_uid mbox position = 
      read_index_uid mbox >>= fun uids ->
      match position with
      | `Sequence seq -> 
        if seq >= List.length uids then
          return `Eof
        else
          return (`Ok (seq,(List.nth_exn uids ((List.length uids) - seq))))
      | `UID uid -> 
        match (List.findi uids ~f:(fun i u -> u = uid)) with 
        | None ->
          if uid > (List.nth_exn uids 0) then
            return `Eof
          else
            return `NotFound
        | Some (seq,uid) ->
            return (`Ok (seq,uid))

    let update_message_metadata mbox position metadata =
      get_uid mbox position >>= function
      | `Eof -> return `Eof
      | `NotFound -> return `NotFound
      | `Ok (_,uid) ->
        IrminIntf.tr_update mbox.trans (get_key (`Metamessage uid))
          (Sexp.to_string (sexp_of_mailbox_message_metadata metadata)) >>= fun () ->
        return `Ok

    let read_message mbox ?filter position =
      get_uid mbox position >>= function
      | `Eof -> return `Eof
      | `NotFound -> return `NotFound
      | `Ok (seq,uid) -> 
        (* redundant search but worth checking if search fails because of the
         * sequence - then don't need to read the record 
         *)
        if filter <> None && (Interpreter.check_search_seq 
            ~seq ~uid (Option.value_exn filter)) = false then
          return `NotFound
        else (
          IrminIntf.tr_read_exn mbox.trans (get_key (`Metamessage uid)) >>= fun sexp_str ->
          let message_metadata = mailbox_message_metadata_of_sexp (Sexp.of_string sexp_str) in
          IrminIntf.tr_read_exn mbox.trans (get_key (`Postmark uid)) >>= fun postmark_sexp_str ->
          IrminIntf.tr_read_exn mbox.trans (get_key (`Email uid)) >>= fun email_sexp_str ->
          let postmark_sexp = Sexp.of_string postmark_sexp_str in
          let postmark = Mailbox.Postmark.t_of_sexp postmark_sexp in
          let email_sexp = Sexp.of_string email_sexp_str in
          let email = Email.t_of_sexp email_sexp in
          let message = {Mailbox.Message.postmark=postmark;Mailbox.Message.email=email} in
          if filter = None || Interpreter.exec_search 
              email (Option.value_exn filter) message_metadata seq = true then
            return (`Ok (message,message_metadata))
          else
            return `NotFound
        )

    let read_message_metadata mbox position =
      get_uid mbox position >>= function
      | `Eof -> return `Eof
      | `NotFound -> return `NotFound
      | `Ok (seq,uid) -> 
        IrminIntf.tr_read_exn mbox.trans (get_key (`Metamessage uid)) >>= fun sexp_str ->
        return (`Ok (mailbox_message_metadata_of_sexp (Sexp.of_string sexp_str)))

    let copy_mailbox mbox1 mbox2 sequence buid =
      return ()

    let delete_message mbox position =
      get_uid mbox position >>= function
      | `Ok (_,uid) -> IrminIntf.tr_remove mbox.trans (get_key (`Uid uid))
      |_ -> return ()

    let list mbox =
      return []

  end
