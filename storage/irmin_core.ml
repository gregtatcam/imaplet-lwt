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
exception InvalidKey of string

module Git = IrminGit.FS(struct
  let root = Some (srv_config.irmin_path)
  let bare = true
end)

module Store = Git.Make(IrminKey.SHA1)(IrminContents.String)(IrminTag.String)


module Key_ :
  sig
    type t

    (* user and unix path to key *)
    val t_of_path : string -> t
    val t_of_list : string list -> t
    val add_path : t -> string -> t
    val add_list : t -> string list -> t
    val create_account : string -> t
    val mailbox_of_path : ?user:string -> string -> (string*bool*t)
    val mailbox_of_list : ?user:string -> string list -> (string*bool*t)
    val mailboxes_of_mailbox : t -> t 
    val key_to_string : t -> string
    val key_to_path : t -> string
    val view_key_to_path : t -> string
    val assert_key : t -> unit
  end with type t = Store.key =
  struct
    type t = Store.key

    let create_account user =
      ["imaplet";user]

    let assert_key key =
      if List.length key = 0 || List.find key ~f:(fun i -> i = "") <> None then
        raise (InvalidKey (String.concat ~sep:"/" key))

    let t_of_path str =
      if str = "" then
        assert(false);
      let path = String.lstrip ~drop:(fun c -> c = '/') str in
      let path = String.rstrip ~drop:(fun c -> c = '/') path in
      String.split path ~on:'/'

    let t_of_list (l:string list) =
      assert_key l;
      l

    let add_path key str =
      List.concat [key;t_of_path str]

    let add_list key l =
      List.concat [key;l]

    let mailbox_of_list ?user l =
      let mailbox,folder,key = List.fold_right l ~init:("",false,[]) 
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
      if user <> None then
        mailbox,folder,"imaplet" :: ((Option.value_exn user) :: key)
      else
        mailbox,folder,key

    (* if user is None then relative path, otherwise root, i.e. /imaplet/user *)
    let mailbox_of_path ?user path =
      let key = String.split path ~on:'/' in
      mailbox_of_list ?user key
      
    let mailboxes_of_mailbox key =
      add_path key "mailboxes"

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
    val create : unit -> store Lwt.t
    val remove : store -> Key_.t -> unit Lwt.t
    val read_exn : store -> Key_.t -> string Lwt.t
    val mem : store -> Key_.t -> bool Lwt.t
    val list : store -> Key_.t -> Store.key list Lwt.t
    val update_view : store -> Key_.t -> Store.View.t -> unit Lwt.t
    val read_view : store -> Key_.t -> Store.View.t Lwt.t
  end =
  struct
    type store = Store.t


    let create () =
      Store.create ()

    let remove store key =
      Key_.assert_key key;
      Store.remove store key

    let read_exn store key =
      Key_.assert_key key;
      Store.read_exn store key

    let mem store key =
      Key_.assert_key key;
      Store.mem store key

    let list store key =
      Key_.assert_key key;
      Store.list store [key]

    let update_view store key view =
      Key_.assert_key key;
      (*Printf.printf "------ store update_view %s\n%!" (Key_.key_to_string * key);*)
      Store.View.update_path store key view

    let read_view store key =
      Key_.assert_key key;
      (*Printf.printf "------ reading view %s\n%!" (Key_.key_to_string key);*)
      Store.View.of_path store key

  end


module IrminIntf_tr :
  sig
    type transaction
    val remove_view : transaction -> unit Lwt.t
    val move_view : transaction -> Key_.t -> unit Lwt.t
    val begin_transaction : Key_.t -> transaction Lwt.t
    val end_transaction : transaction -> unit Lwt.t
    val update : transaction -> Key_.t -> string -> unit Lwt.t
    val read : transaction -> Key_.t -> string option Lwt.t
    val read_exn : transaction -> Key_.t -> string Lwt.t
    val list : transaction -> Key_.t -> Store.key list Lwt.t
    val remove : transaction -> Key_.t -> unit Lwt.t
    val mem : transaction -> Key_.t -> bool Lwt.t
  end =
  struct
    type transaction = Store.t * Store.View.t * Key_.t * bool ref

    let begin_transaction key =
      Key_.assert_key key;
      Store.create () >>= fun store ->
      (*Printf.printf "------ creating view %s\n%!" (Key_.key_to_string key);*)
      Store.View.of_path store key >>= fun view ->
      return (store,view,key,ref false)

    let end_transaction tr =
      let (store,view,key,dirty) = tr in
      if !dirty = true then (
        (*Printf.printf "++++++++++++++++++ commiting %s!!!\n%!"
        (Key_.key_to_string key);*)
        Store.View.update_path store key view >>= fun () ->
        dirty := false;
        return ()
      ) else
        return ()

    let remove_view tr =
      let (store,_,key,_) = tr in
      Store.remove store key

    let move_view tr key2 =
      let (store,view,_,_) = tr in
      Store.View.update_path store key2 view


    let update tr key data =
      Key_.assert_key key;
      (*Printf.printf "------ store view.update %s\n" (Key_.key_to_string * key);*)
      let (_,view,_,dirty) = tr in
      Store.View.update view key data >>= fun () ->
      dirty := true;
      return ()

    let read tr key =
      Key_.assert_key key;
      let (_,view,_,_) = tr in
      Store.View.read view key

    let read_exn tr key =
      Key_.assert_key key;
      let (_,view,_,_) = tr in
      Store.View.read_exn view key

    let list tr key =
      Key_.assert_key key;
      (*Printf.printf "------ store list %s\n%!" (Key_.key_to_string key);*)
      let (_,view,_,_) = tr in
      Store.View.list view [key]

    let remove tr key =
      Key_.assert_key key;
      (*Printf.printf "------ store remove %s\n" (Key_.key_to_string key);*)
      let (_,view,_,dirty) = tr in
      Store.View.remove view key >>= fun () ->
      dirty := true;
      return ()

    let mem tr key =
      Key_.assert_key key;
      let (_,view,_,_) = tr in
      Store.View.mem view key

    let tr_key tr =
      let (_,_,key,_) = tr in
      key

  end

(* mailboxes subscription *)
module Subscriptions :
  sig
    type t
    val key_subscr : Key_.t
    val create : string -> t Lwt.t
    val read : t -> string list Lwt.t
    val subscribe : t -> string -> unit Lwt.t
    val unsubscribe : t -> string -> unit Lwt.t
    val empty : string
  end =
  struct
    type t = IrminIntf_tr.transaction

    let key_subscr = Key_.t_of_path "subscriptions"

    (* create type *)
    let create user =
      let key = Key_.create_account user in
      IrminIntf_tr.begin_transaction key

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
      IrminIntf_tr.mem view key_subscr >>= fun res ->
      if res = false then raise KeyDoesntExist;
      let str = str_sexp_of_list l in
      IrminIntf_tr.update view key_subscr str >>
      IrminIntf_tr.end_transaction view

    (* read subscription *)
    let read view =
      IrminIntf_tr.read view key_subscr >>= function
      | Some str -> return (list_of_str_sexp str)
      | None -> return []

    (* subscribe *)
    let subscribe t mailbox =
      read t >>= fun l ->
      if (List.find l ~f:(fun i -> i = mailbox) <> None) then 
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
    type t = Key_.t

    (* create type *)
    let create user = 
      Key_.create_account user

    (* create new account *)
    let create_account key =
      IrminIntf_tr.begin_transaction key >>= fun view ->
      IrminIntf_tr.mem view Subscriptions.key_subscr >>= fun res ->
      if res then
        return `Exists
      else (
        IrminIntf_tr.update view Subscriptions.key_subscr Subscriptions.empty >>
        IrminIntf_tr.end_transaction view >>
        return `Ok
      )

    (* remove account *)
    let delete_account key =
      IrminIntf.create () >>= fun store ->
      IrminIntf.remove store key
      (*
      IrminIntf.begin_transaction key >>= fun view ->
      IrminIntf.remove view Subscriptions.key_subscr >>
      IrminIntf.remove view (Key_.add_path key "mailboxes") >>
      IrminIntf.end_transaction view
      *)

  end

  type mailbox_ = {user:string;mailbox:string;folders:bool;trans:IrminIntf_tr.transaction;
      index:int list option ref}

(* consistency TBD *)
module IrminMailbox :
  sig
    type t
    val create : string -> string  -> t Lwt.t
    val commit : t -> unit Lwt.t
    val exists : t -> [`No|`Folder|`Mailbox] Lwt.t
    val exists_path : t -> string -> [`No|`Folder|`Mailbox] Lwt.t
    val exists_key : t -> Key_.t -> [`No|`Folder|`Mailbox] Lwt.t
    val create_mailbox : t -> unit Lwt.t
    val delete_mailbox : t -> unit Lwt.t
    val move_mailbox : t -> string -> unit Lwt.t
    val copy_mailbox : t -> [`Sequence of int|`UID of int] -> t ->
      mailbox_message_metadata -> unit Lwt.t
    val read_mailbox_metadata : t -> mailbox_metadata Lwt.t
    val append_message : t -> Mailbox.Message.t -> mailbox_message_metadata -> unit Lwt.t
    val update_mailbox_metadata : t -> mailbox_metadata -> unit Lwt.t
    val update_message_metadata : t -> [`Sequence of int|`UID of int] -> mailbox_message_metadata ->
      [`NotFound|`Eof|`Ok] Lwt.t
    val read_message : t -> ?filter:(searchKey) searchKeys -> 
      [`Sequence of int|`UID of int] ->
      [`NotFound|`Eof|`Ok of (Mailbox.Message.t * mailbox_message_metadata)] Lwt.t
    val read_message_raw : t -> ?filter:(searchKey) searchKeys -> 
      [`Sequence of int|`UID of int] ->
      [`NotFound|`Eof|`Ok of (Mailbox.Message.t option * string * string * string * mailbox_message_metadata)] Lwt.t
    val read_message_metadata : t -> [`Sequence of int|`UID of int] -> 
      [`NotFound|`Eof|`Ok of mailbox_message_metadata] Lwt.t
    val delete_message : t -> [`Sequence of int|`UID of int] -> unit Lwt.t
    val list : t -> subscribed:bool -> ?access:(string -> bool) -> init:'a -> 
      f:('a -> [`Folder of string*int|`Mailbox of string] -> 'a Lwt.t) -> 'a Lwt.t
    val read_index_uid : t -> int list Lwt.t
    val show_all : t -> unit Lwt.t
    val uid_to_seq : t -> int -> int option Lwt.t
  end with type t = mailbox_ = 
  struct 
    (* user * mailbox * is-folder * irmin key including the mailbox *)
    type t = mailbox_

    let get_key = function
    | `Metamailbox -> Key_.t_of_path "meta"
    | `Index -> Key_.t_of_path "index"
    | `Messages -> Key_.t_of_path "messages"
    | `Postmark uid -> Key_.t_of_path ("messages/" ^ (string_of_int uid) ^ "/postmark")
    | `Headers uid -> Key_.t_of_path ("messages/" ^ (string_of_int uid) ^ "/headers")
    | `Email uid -> Key_.t_of_path ("messages/" ^ (string_of_int uid) ^ "/email")
    | `Metamessage uid -> Key_.t_of_path ("messages/" ^ (string_of_int uid) ^ "/meta")
    | `Uid uid -> Key_.t_of_path ("messages/" ^ (string_of_int uid))

    (* commit should be called explicitly on each created mailbox to have the
     * changes commited to Irmin
     *)
    let create user path =
      let (mailbox,folders,key) = Key_.mailbox_of_path ?user:(Some user) path in
      IrminIntf_tr.begin_transaction key >>= fun trans ->
        return {user;mailbox;folders;trans;index=ref None}

    let commit mbox =
      IrminIntf_tr.end_transaction mbox.trans

    (* create mailbox metadata and index stores *)
    let create_mailbox mbox =
      let key = get_key `Metamailbox in
      let metadata = empty_mailbox_metadata ~uidvalidity:(new_uidvalidity())()
        ~folders:mbox.folders in
      let sexp = sexp_of_mailbox_metadata metadata in
      IrminIntf_tr.update mbox.trans key (Sexp.to_string sexp) >>= fun () ->
      let key = get_key `Index in
      let sexp = List.sexp_of_t (fun i -> Sexp.of_string i) [] in
      IrminIntf_tr.update mbox.trans key (Sexp.to_string sexp) 

    let delete_mailbox mbox =
      IrminIntf_tr.remove_view mbox.trans

    (* how to make this the transaction? TBD *)
    let move_mailbox mbox path =
      let (_,_,key2) = Key_.mailbox_of_path ?user:(Some mbox.user) path in
      IrminIntf_tr.move_view mbox.trans key2 >>
      IrminIntf_tr.remove_view mbox.trans

    let read_mailbox_metadata mbox =
      IrminIntf_tr.read_exn mbox.trans (get_key `Metamailbox) >>= fun sexp_str ->
      let sexp = Sexp.of_string sexp_str in
      return (mailbox_metadata_of_sexp sexp)

    let update_mailbox_metadata mbox metadata =
      (*Printf.printf "updating mailbox metadata %d\n%!" metadata.uidnext;*)
      let sexp = sexp_of_mailbox_metadata metadata in
      let key = get_key `Metamailbox in
      IrminIntf_tr.update mbox.trans key (Sexp.to_string sexp)

    let exists mbox =
      catch (fun () ->
        read_mailbox_metadata mbox >>= fun metadata ->
        return (if metadata.folders then `Folder else `Mailbox)
      )
      (fun _ -> return `No)

    let exists_key mbox key =
      catch (fun () ->
        let key = Key_.add_path key "meta" in
        IrminIntf_tr.read_exn mbox.trans key >>= fun metadata_sexp_str ->
        let metadata_sexp = Sexp.of_string metadata_sexp_str in
        let metadata = mailbox_metadata_of_sexp metadata_sexp in
        return (if metadata.folders then `Folder else `Mailbox)
      )
      (fun _ -> return `No)

    let exists_path mbox path =
      let (_,_,key) = Key_.mailbox_of_path path in
      exists_key mbox key

    let find_flag l fl =
      List.find l ~f:(fun f -> f = fl) <> None

    let read_index_uid mbox =
      match mbox.!index with
      | None ->
        IrminIntf_tr.read_exn mbox.trans (get_key `Index) >>= fun index_sexp_str ->
        let uids = (List.t_of_sexp (fun i -> int_of_string (Sexp.to_string i)) 
            (Sexp.of_string index_sexp_str)) in
        mbox.index := Some uids;
        return uids
      | Some uids -> return uids

    let update_index_uids mbox uids = 
      mbox.index := Some uids;
      IrminIntf_tr.update mbox.trans (get_key `Index) 
        (Sexp.to_string (List.sexp_of_t (fun i -> Sexp.of_string (string_of_int i)) (uids)))

    let update_index_uid mbox uid =
      read_index_uid mbox >>= fun uids ->
      if List.find uids ~f:(fun u -> u = uid) <> None then
        raise DuplicateUID
      else (
        (* reverse index *)
        let uids = (uid :: uids) in
        update_index_uids mbox uids
      )

    let uid_to_seq mbox uid =
      read_index_uid mbox >>= fun uids ->
      match (List.findi uids ~f:(fun i u -> u = uid)) with
      | None -> return None
      | Some (i,_) -> return (Some ((List.length uids) - i))

    let append_message_raw mbox ~postmark_sexp_str ~headers_sexp_str ~email_sexp_str 
        message_metadata =
      let uid = message_metadata.uid in
      IrminIntf_tr.update mbox.trans (get_key (`Metamessage uid)) 
        (Sexp.to_string (sexp_of_mailbox_message_metadata message_metadata)) >>
      IrminIntf_tr.update mbox.trans (get_key (`Postmark uid)) postmark_sexp_str >>
      IrminIntf_tr.update mbox.trans (get_key (`Email uid)) email_sexp_str >>
      IrminIntf_tr.update mbox.trans (get_key (`Headers uid)) headers_sexp_str >>
      update_index_uid mbox message_metadata.uid >>
      return ()

    let append_message mbox message message_metadata =
      let postmark_sexp_str = Sexp.to_string (Mailbox.Postmark.sexp_of_t message.postmark) in
      let email_sexp_str = Sexp.to_string (Email.sexp_of_t message.email) in
      let headers_sexp_str = Sexp.to_string (Header.sexp_of_t (Email.header message.email)) in
      append_message_raw mbox ~postmark_sexp_str ~headers_sexp_str ~email_sexp_str 
          message_metadata

    let get_uid mbox position = 
      read_index_uid mbox >>= fun uids ->
      match position with
      | `Sequence seq -> 
        if seq > List.length uids then
          return `Eof
        else if seq = 0 then
          return `NotFound
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
            return (`Ok ((List.length uids) - seq,uid))

    let update_message_metadata mbox position metadata =
      get_uid mbox position >>= function
      | `Eof -> return `Eof
      | `NotFound -> return `NotFound
      | `Ok (_,uid) ->
        IrminIntf_tr.update mbox.trans (get_key (`Metamessage uid))
          (Sexp.to_string (sexp_of_mailbox_message_metadata metadata)) >>= fun () ->
        return `Ok

    let get_message_only_raw mbox uid =
      IrminIntf_tr.read_exn mbox.trans (get_key (`Headers uid)) >>= fun headers_sexp_str ->
      IrminIntf_tr.read_exn mbox.trans (get_key (`Postmark uid)) >>= fun postmark_sexp_str ->
      IrminIntf_tr.read_exn mbox.trans (get_key (`Email uid)) >>= fun email_sexp_str ->
      return (postmark_sexp_str, headers_sexp_str, email_sexp_str)

    let read_message_raw mbox ?filter position =
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
          IrminIntf_tr.read_exn mbox.trans (get_key (`Metamessage uid)) >>= fun sexp_str ->
          let message_metadata = mailbox_message_metadata_of_sexp (Sexp.of_string sexp_str) in
          get_message_only_raw mbox uid >>=
          fun (postmark_sexp_str, headers_sexp_str, email_sexp_str) ->
          if filter = None then
            return (`Ok (None,postmark_sexp_str,headers_sexp_str,email_sexp_str,message_metadata))
          else (
            let postmark = Mailbox.Postmark.t_of_sexp (Sexp.of_string postmark_sexp_str) in
            let email = Email.t_of_sexp (Sexp.of_string email_sexp_str) in
            let message = {Mailbox.Message.postmark=postmark;Mailbox.Message.email=email} in
            if Interpreter.exec_search email (Option.value_exn filter) message_metadata seq = true then
              return (`Ok (Some message,postmark_sexp_str,headers_sexp_str,email_sexp_str,message_metadata))
            else
              return `NotFound
          )
        )

    let read_message mbox ?filter position =
      read_message_raw mbox ?filter position >>= function
      | `Eof -> return `Eof
      | `NotFound -> return `NotFound
      | `Ok (message,postmark_sexp_str,_,email_sexp_str,message_metadata) ->
          if message <> None then
            return (`Ok (Option.value_exn message,message_metadata))
          else (
            let postmark = Mailbox.Postmark.t_of_sexp (Sexp.of_string postmark_sexp_str) in
            let email = Email.t_of_sexp (Sexp.of_string email_sexp_str) in
            let message = {Mailbox.Message.postmark=postmark;Mailbox.Message.email=email} in
            return (`Ok (message,message_metadata))
          )

    let read_message_metadata mbox position =
      get_uid mbox position >>= function
      | `Eof -> return `Eof
      | `NotFound -> return `NotFound
      | `Ok (seq,uid) -> 
        IrminIntf_tr.read_exn mbox.trans (get_key (`Metamessage uid)) >>= fun sexp_str ->
        return (`Ok (mailbox_message_metadata_of_sexp (Sexp.of_string sexp_str)))

    let get_min_max_uid uids =
      if List.length uids = 0 then
        (0,0)
      else
        (List.nth_exn uids 0),(List.last_exn uids)

    let get_min_max_seq mailbox_metadata =
      (1, mailbox_metadata.count)

    let get_min_max mailbox_metadata uids buid =
      if buid then
        get_min_max_uid uids
      else
        get_min_max_seq mailbox_metadata

    let copy_mailbox mbox1 pos mbox2 message_metadata =
      get_uid mbox1 pos >>= function
      | `Eof -> return ()
      | `NotFound -> return ()
      | `Ok (seq,uid) ->
        get_message_only_raw mbox1 uid >>=
        fun (postmark_sexp_str,headers_sexp_str,email_sexp_str) ->
        append_message_raw mbox2 ~postmark_sexp_str ~headers_sexp_str
        ~email_sexp_str message_metadata 

    let delete_message mbox position =
      get_uid mbox position >>= function
      | `Ok (_,uid) -> 
        IrminIntf_tr.remove mbox.trans (get_key (`Postmark uid)) >>
        IrminIntf_tr.remove mbox.trans (get_key (`Headers uid)) >>
        IrminIntf_tr.remove mbox.trans (get_key (`Email uid)) >>
        IrminIntf_tr.remove mbox.trans (get_key (`Metamessage uid)) >>
        IrminIntf_tr.remove mbox.trans (get_key (`Uid uid)) >>
        read_index_uid mbox >>= fun uids ->
        let uids = List.fold_right uids ~init:[] ~f:(fun u uids ->
          if u = uid then
            uids
          else
            u :: uids
        ) in
        update_index_uids mbox uids
      |_ -> return ()

    let list_mailbox mbox key =
      IrminIntf_tr.list mbox.trans (Key_.mailboxes_of_mailbox key)

    let is_folder mbox key =
      exists_key mbox key >>= function
      | `Folder -> return true
      | _ -> return false

    let rec list_ mbox key subscriptions access init f =
      list_mailbox mbox key >>= fun listing ->
      Lwt_list.fold_left_s (fun (acc,cnt) name ->
        (* if folder is not accessible then neither are the children *)
        (* need to handle subscriptions TBD *)
        if access (Key_.view_key_to_path key) = false then
          return (acc,cnt)
        else (
          let key = Key_.t_of_list name in
          is_folder mbox key >>= fun res ->
          begin
          if res = true then (
            list_ mbox key subscriptions access acc f >>= fun (acc,cnt) -> 
            f acc (`Folder ((Key_.view_key_to_path key),cnt)) 
          ) else (
            f acc (`Mailbox (Key_.view_key_to_path key)) 
          )
          end >>= fun (acc) ->
          return (acc,cnt+1)
        )
      ) (init,0) listing

    (* assuming that "reference" and "mailbox" arguments in list command
     * are converted into initial mailbox and regular expression to match
     * final mailbox to list is concat of the reference and mailbox minus wild
     * cards starting with the first delimeter followed by the wild card
     *)
    let list mbox ~subscribed ?(access=(fun _ -> true)) ~init ~f =
      let (_,_,key) = Key_.mailbox_of_path "" in
      begin
      if subscribed then (
        Subscriptions.create mbox.user >>= fun sub ->
        Subscriptions.read sub >>= fun sub -> return (Some sub)
      ) else (
        return None
      ) 
      end >>= fun subscriptions ->
      list_ mbox key subscriptions access init f >>= fun (acc,_) ->
      return acc

    let list_messages k =
      let open Core.Std in
      let list_subtr store k =
        IrminIntf.list store k >>= fun l ->
        return (List.fold l ~init:"" ~f:(fun acc i ->
          acc ^ ":" ^ (List.last_exn i)
        ))
      in
      IrminIntf.create () >>= fun store ->
      IrminIntf.list store k >>= fun l ->
      Lwt_list.fold_left_s (fun acc i ->
        let k = Key_.t_of_list ((List.concat [k;i])) in
        list_subtr store k >>= fun s ->
        return (((Key_.key_to_string (Key_.t_of_list i)) ^ ":" ^ s) :: acc)
      ) [] l

    let show_all mbox =
      let open Core.Std in
      Printf.printf "---------- mailbox messages\n%!";
      let (_,_,key) = Key_.mailbox_of_path mbox.mailbox in
      let key = Key_.add_path key "messages" in
      list_messages key >>= fun l ->
      List.iter l ~f:(fun i -> Printf.printf "%s %!" i); Printf.printf "\n%!";
      Printf.printf "---------- mailbox index\n%!";
      read_index_uid mbox >>= fun uids ->
      List.iter uids ~f:(fun i -> Printf.printf "%d %!" i); Printf.printf "\n%!";
      Printf.printf "---------- mailbox metadata\n%!";
      read_mailbox_metadata mbox >>= fun meta ->
      Printf.printf "%s\n%!" (Sexp.to_string (sexp_of_mailbox_metadata meta));
      Printf.printf "---------- subscriptions\n%!";
      Subscriptions.create mbox.user >>= fun sub ->
      Subscriptions.read sub >>= fun subscr ->
      List.iter subscr ~f:(fun i -> Printf.printf "%s %!" i); Printf.printf "\n%!";
      return ()

  end
