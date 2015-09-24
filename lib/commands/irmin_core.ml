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
open Sexplib
open Server_config
open Imaplet_types
open Storage_meta
open Utils
open Sexplib.Conv
open Lazy_message
open Lazy_maildir_message
open Parsemail

exception KeyDoesntExist
exception DuplicateUID
exception InvalidKey of string
exception EmptyPrivateKey
exception InvalidUid of int

module Store = Irmin.Basic (Irmin_git.FS) (Irmin.Contents.String)
module View = Irmin.View(Store)
module MapStr = Map.Make(String)

type hashes = {attachments:int} with sexp

type irmin_accessors =
  (unit -> string Lwt.t) * (* postmark *)
  (unit -> (Email_parse.email_map * string) Lwt.t) * (* headers *)
  (unit -> string Lwt.t) * (* content *)
  (string -> string Lwt.t) * (* attachment *) 
  (unit -> mailbox_message_metadata Lwt.t)

type irmin_accessors_rec = {
  postmark:string Lwt.t lazy_t;
  headers:(Email_parse.email_map * string) Lwt.t lazy_t;
  content:string Lwt.t lazy_t;
  attachment:(string -> string Lwt.t);
  metadata:mailbox_message_metadata Lwt.t lazy_t;
}

type email_irmin_accessors =
  Email_parse.email_map * string * (string Lwt.t lazy_t) * 
    (string Lwt.t lazy_t Map.Make(String).t)

module LazyIrminEmail : LazyEmail_intf with type c = email_irmin_accessors =
  struct 
    open Email_parse
    type c = email_irmin_accessors
    type t = {
      email:email_map;
      headers: string;
      content: string Lwt.t lazy_t;
      attachment: string Lwt.t lazy_t Map.Make(String).t;
    }

    let empty = {
      email = {part={size=0;lines=0};header={offset=0;length=0};content=`Data_map {offset=0;length=0}};
      headers = "";
      content = Lazy.from_fun (fun () -> return "");
      attachment = MapStr.empty;
    }

    let create c =
      let (email,headers,content,attachment) = c in
      {email;headers;content;attachment}

    let header ?(incl=`Map MapStr.empty) ?(excl=MapStr.empty) t =
      let sexp = Sexp.of_string (Bytes.sub t.headers t.email.header.offset t.email.header.length) in
      return(
      List.filter (fun (n,_) ->
        let dont_incl =
        match incl with
        | `Map incl -> 
          MapStr.is_empty incl = false && MapStr.mem (String.lowercase n) incl = false
        | `Regx incl ->
          Regex.match_regex ~case:false ~regx:incl n = false
        in
        (* exclude if it is not on included list or is on excluded list *)
        (dont_incl = true ||
          MapStr.is_empty excl = false && MapStr.mem (String.lowercase n) excl = true) = false
      ) (list_of_sexp (fun sexp -> pair_of_sexp string_of_sexp string_of_sexp sexp) sexp))

    let header_to_str ?(incl=`Map MapStr.empty) ?(excl=MapStr.empty) t =
      header ~incl ~excl t >>= fun hdr ->
      return (String.concat "" (List.fold_right (fun (n,v) acc-> List.concat
        [[n;":";v;crlf];acc]) hdr []))

    let convert t =
      match t.email.content with
      | `Data_map dd -> 
        Lazy.force t.content >>= fun content ->
        return (`Data (Bytes.sub content dd.offset dd.length)) 
      | `Attach_map contid -> 
        Lazy.force (MapStr.find contid t.attachment) >>= fun attachment ->
        return (`Data attachment)
      | `Message_map email -> 
        return (`Message {t with email})
      | `Multipart_map (boundary,lemail) -> 
        return (`Multipart_ (boundary,(List.map (fun email -> {t with email}) lemail)))

    let content t =
      convert t >>= fun c ->
      match c with
      | `Data d -> return (`Data d)
      | `Message m -> return (`Message m)
      | `Multipart_ (_,lemail) -> return (`Multipart lemail)

    let raw_content t =
      let buffer = Buffer.create 100 in
      let add_string ?(crlf=false) str = 
        Buffer.add_string buffer str;
        if crlf then Buffer.add_string buffer Regex.crlf 
      in
      let rec _raw_content t with_header last_crlf =
        let header t buffer with_header = 
          if with_header then
            add_string ~crlf:true (header_of_sexp_str (Bytes.sub t.headers t.email.header.offset t.email.header.length))
        in
        convert t >>= fun c ->
        header t buffer with_header;
        match c with
        | `Data data -> add_string ~crlf:true data; return ()
        | `Message email -> _raw_content email true crlf
        | `Multipart_ (boundary,lemail) ->
          add_string crlf;
          Lwt_list.iter_s (fun email ->
            add_string ~crlf:true boundary;
            _raw_content email true crlf
          ) lemail >>= fun () ->
          add_string boundary;
          add_string ~crlf:true "--";
          add_string last_crlf;
          return ()
      in
      _raw_content t false "" >>
      return (Buffer.contents buffer)

    let to_string ?(incl=`Map MapStr.empty) ?(excl=MapStr.empty) t =
      header_to_str ~incl ~excl t >>= fun headers ->
      raw_content t >>= fun content ->
      return (String.concat crlf [headers ; content])

    let size t =
      return t.email.part.size

    let lines t =
      return t.email.part.lines

  end

module LazyIrminMessage : LazyMessage_intf with type c = irmin_accessors =
  struct
    open Email_parse

    type c = irmin_accessors

    type t = irmin_accessors_rec

    let create c =
      let (postmark,headers,content,attachment,metadata) = c in
      {postmark=Lazy.from_fun postmark;
       headers=Lazy.from_fun headers;
       content=Lazy.from_fun content;
       attachment;
       metadata=Lazy.from_fun metadata}

    let get_postmark t =
      Lazy.force t.postmark

    let get_headers_block t =
      Lazy.force t.headers >>= fun (_,headers) ->
      return headers

    let get_content_block (t:irmin_accessors_rec) =
      Lazy.force t.content

    let get_email t =
      let rec walk email acc =
        match email.content with
        | `Data_map d -> acc
        | `Attach_map contid -> MapStr.add contid (Lazy.from_fun (fun () -> t.attachment contid)) acc
        | `Message_map email -> walk email acc
        | `Multipart_map (_,lemail) -> 
          List.fold_left (fun acc email -> walk email acc) acc lemail
      in
      Lazy.force t.headers >>= fun (email,headers) ->
      return (build_lazy_email_inst (module LazyIrminEmail)
        (email,headers,t.content,walk email MapStr.empty))

    let get_message_metadata t =
      Lazy.force t.metadata
  end

module Key_ :
  sig
    type t

    (* user and unix path to key *)
    val t_of_path : string -> t
    val t_of_list : string list -> t
    val add_path : t -> string -> t
    val add_list : t -> string list -> t
    val create_account : string -> t
    val mailbox_of_path : string -> (string*t)
    val mailbox_of_list : string list -> (string*t)
    val mailboxes_of_mailbox : t -> t 
    val key_to_string : t -> string
    val key_to_path : t -> string
    val view_key_to_path : t -> string
    val key_to_workpath : t -> string
    val view_key_to_workpath : t -> string
    val assert_key : t -> unit
  end with type t = View.key =
  struct
    type t = View.key

    let create_account user =
      ["imaplet";user]

    let assert_key key =
      if List.length key = 0 || list_find key (fun i -> i = "") then
        raise (InvalidKey (String.concat "/" key))

    let t_of_path str =
      if str = "" then
        assert(false);
      let path = Regex.replace ~regx:"^/" ~tmpl:"" str in
      let path = Regex.replace ~regx:"/$" ~tmpl:"" path in
      Str.split (Str.regexp "/") path

    let t_of_list (l:string list) =
      assert_key l;
      l

    let add_path key str =
      List.concat [key;t_of_path str]

    let add_list key l =
      List.concat [key;l]

    let mailbox_of_list l =
      List.fold_right (fun i (mailbox,acc) -> 
        if i <> "" then (
          if List.length acc <> 0 then
            mailbox,"mailboxes" :: (i :: acc)
          else
            i,"mailboxes" :: (i :: acc)
        ) else
          mailbox,acc
      ) l ("",[])

    (* if user is None then relative path, otherwise root, i.e. /imaplet/user *)
    let mailbox_of_path path =
      let key = Str.split (Str.regexp "/") path in
      mailbox_of_list key
      
    let mailboxes_of_mailbox key =
      add_path key "mailboxes"

    (* convert key to path, keep "imaplet", user, and "mailboxes" *)
    let key_to_string key = "/" ^ (String.concat "/" key)

    (* convert key to path, remove "imaplet",user, and "mailboxes" *)
    let key_to_path key = 
     let (_,acc) =
     List.fold_left
     (fun (i,acc) item -> 
       if i < 2 then (* skip imaplet and user *)
         (i+1,acc)
       else if (Pervasives.(mod) i 2) = 0 then ( (* skip mailboxes *) 
         if acc = "" then
           (i+1,acc)
         else
           (i+1,acc ^ "/") 
       ) else 
         (i+1,acc ^ item)
     ) (0,"") key
     in
     acc

    (* convert key to git workdir path, keep all components *)
    let key_to_workpath key = 
      String.concat "/" key

    (* convert view key (relative key) to path, remove "mailboxes" *)
    let view_key_to_path key = 
     let (_,acc) =
     List.fold_left
     (fun (i,acc) item -> 
       if (Pervasives.(mod) i  2) = 0 then ( (* skip mailboxes *) 
         if acc = "" then
           (i+1,acc)
         else
           (i+1,acc ^ "/") 
       ) else 
         (i+1,acc ^ item)
     ) (0,"") key
     in
     acc

    (* convert view key (relative key) to workdir path, keep all components *)
    let view_key_to_workpath key = 
      String.concat "/" key

  end

let get_irmin_path user config =
  match user with
  | None -> config.irmin_path
  | Some user -> Utils.user_path ~user ~path:config.irmin_path ()

module type GitIntf =
  sig
    type store
    type view
    val create : ?user:string -> Server_config.imapConfig -> store Lwt.t
    val remove : store -> Key_.t -> unit Lwt.t
    val read_exn : store -> Key_.t -> string Lwt.t
    val mem : store -> Key_.t -> bool Lwt.t
    val list : store -> Key_.t -> Key_.t list Lwt.t
    val update : store -> Key_.t -> string -> unit Lwt.t
    val update_view : store -> Key_.t -> view -> unit Lwt.t
    val read_view : store -> Key_.t -> view Lwt.t
  end

(* workdir creates and updates files in git working directory
 * the files (metadata, indeces, not messages) are overwritten! this is conceptually implementation of maildir
 *)
module GitWorkdirIntf : GitIntf with type store = string 
  and type view = string =
  struct
    type store = string
    type view = string (* reference to path *)

    let get_path store key =
      Filename.concat store (Key_.key_to_workpath key)

    let create ?user config =
      return (get_irmin_path user config)

    let remove store key =
      let path = get_path store key in
      exists path S_REG >>= fun res ->
      if res then
        Lwt_unix.unlink path
      else (
        Lwt_unix.system ("rm -rf " ^ path) >>= fun _ ->
        return ()
      )

    let read_exn store key =
      let path = get_path store key in
      with_file ~lock:true path ~flags:[O_RDONLY] ~perms:0o664
      ~mode:Lwt_io.Input ~f:(fun ch ->
        Lwt_io.read ch
      )

    let mem store key =
      let path = get_path store key in
      exists path ~alt:S_DIR S_REG 

    let list store key =
      let path = get_path store key in
      files_of_directory path (fun acc file -> 
        let key = Key_.add_list key [file] in
        return (key :: acc)
      ) [] >>= function
      | `Ok l -> return l
      | `NoDir -> return [] (* directory doesn't exist -> mailbox with no submailboxes *)

    let update store key data =
      let path = get_path store key in
      let base = Regex.replace ~regx:" " ~tmpl:"\\ " (Filename.dirname path) in
      exists base S_DIR >>= fun res ->
      begin
      if res = false then (
        Lwt_unix.system ("mkdir -p " ^ base) >>= fun _ ->
        Lwt_unix.system ("chmod 775 " ^ base) >>= fun _ ->
        return ()
      ) else (
        return ()
      )
      end >>
      with_file ~lock:true path ~flags:[O_CREAT;O_TRUNC;O_WRONLY] ~perms:0o664
      ~mode:Lwt_io.Output ~f:(fun ch ->
        Lwt_io.write ch data
      )

    (* update_view doesn't do anything in workdir
     * all updates happen by individual updates in GitIntf_tr
     *)
    let update_view store key v =
      return ()

    (* just return the path to the view *)
    let read_view store key = 
      return (Key_.view_key_to_workpath key)

  end

module IrminIntf : GitIntf with type store = (string -> View.db) and 
  type view = View.t =
  struct
    type store = (string -> View.db)
    type view = View.t

    let fmt t x = Printf.ksprintf (fun str -> t str) x
    let path () = String.concat "/"

    let task msg =
      let date = Int64.of_float (Unix.gettimeofday ()) in
      let owner = "imaplet <imaplet@openmirage.org>" in
      Irmin.Task.create ~date ~owner msg

    let create ?user config =
      let _config = Irmin_git.config 
        ~root:(get_irmin_path user config)
        ~bare:(config.irmin_expand=false) () in
      Store.create _config task 

    let remove store key =
      Key_.assert_key key;
      Store.remove_rec (fmt store "Remove %a." path key) key

    let read_exn store key =
      Key_.assert_key key;
      Store.read_exn (fmt store "Read %a." path key) key

    let mem store key =
      Key_.assert_key key;
      Store.mem (fmt store "Check if %a exists." path key) key

    let list store key =
      Key_.assert_key key;
      Store.list (fmt store "List the contents of %a" path key) key

    let update store key data =
      Store.update (fmt store "Update %a." path key) key data

    let update_view store key view =
      Key_.assert_key key;
      let msg =
        let buf = Buffer.create 1024 in
        let path buf key = Buffer.add_string buf (String.concat "/" key) in
        Printf.bprintf buf "Updating %a.\n\n" path key;
        let actions = View.actions view in
        List.iter (function
            | `List (k, _)     -> Printf.bprintf buf "- list   %a\n" path k
            | `Read (k, _)     -> Printf.bprintf buf "- read   %a\n" path k
            | `Rmdir k         -> Printf.bprintf buf "- rmdir  %a\n" path k
            | `Write (k, None) -> Printf.bprintf buf "- remove %a\n" path k
            | `Write (k, _)    -> Printf.bprintf buf "- write  %a\n" path k
          ) actions;
        Buffer.contents buf
      in
      View.merge_path_exn (store msg) key view

    let read_view store key =
      Key_.assert_key key;
      View.of_path (fmt store "Reading %a" path key) key

  end

module type GitIntf_tr =
  sig
    type transaction
    val remove_view : transaction -> unit Lwt.t
    val move_view : transaction -> Key_.t -> unit Lwt.t
    val begin_transaction : ?user:string -> Server_config.imapConfig -> Key_.t -> transaction Lwt.t
    val end_transaction : transaction -> unit Lwt.t
    val update : transaction -> Key_.t -> string -> unit Lwt.t
    val read : transaction -> Key_.t -> string option Lwt.t
    val read_exn : transaction -> Key_.t -> string Lwt.t
    val list : transaction -> Key_.t -> Key_.t list Lwt.t
    val remove : transaction -> Key_.t -> unit Lwt.t
    val mem : transaction -> Key_.t -> bool Lwt.t
  end

module GitWorkdirIntf_tr : GitIntf_tr with type transaction =
  GitWorkdirIntf.store * string * Key_.t * bool ref =
  struct 
    type transaction = GitWorkdirIntf.store * string * Key_.t * bool ref

    let begin_transaction ?user config key =
      GitWorkdirIntf.create ?user config >>= fun store ->
      GitWorkdirIntf.read_view store key >>= fun view ->
      return (store, view, key, ref false)

    let end_transaction tr =
      let (store,view,key,dirty) = tr in
      if !dirty = true then (
        GitWorkdirIntf.update_view store key view >>= fun () ->
        dirty := false;
        return ()
      ) else
        return ()

    let remove_view tr =
      let (store,_,key,_) = tr in
      GitWorkdirIntf.remove store key

    let move_view tr key2 =
      let (store,view,_,_) = tr in
      Lwt_unix.rename (Filename.concat store view)
        (Filename.concat (Key_.key_to_workpath key2) view)

    let add_key_to_view view key =
      Key_.add_list (Key_.t_of_path view) key

    let update tr key data =
      Key_.assert_key key;
      let (store,view,_,dirty) = tr in
      GitWorkdirIntf.update store (add_key_to_view view key) data >>= fun () ->
      dirty := true;
      return ()

    let read_exn tr key =
      Key_.assert_key key;
      let (store,view,_,_) = tr in
      GitWorkdirIntf.read_exn store (add_key_to_view view key)

    let read tr key =
      catch (fun () -> 
        read_exn tr key >>= fun res -> 
        return (Some res)
      ) (fun _ -> return None)

    let list tr key =
      Key_.assert_key key;
      let (store,view,_,_) = tr in
      (* view starts at imaplet/user *)
      let path = Key_.key_to_workpath (add_key_to_view view key) in
      let path = Filename.concat store path in
      files_of_directory path (fun acc file ->
        (* include the parent directory (key) relative to the view *)
        let key = Key_.add_list key [file] in
        return (key :: acc)
      ) [] >>= function
      | `Ok l -> return l
      | `NoDir -> return []

    let remove tr key =
      Key_.assert_key key;
      let (store,view,_,dirty) = tr in
      GitWorkdirIntf.remove store (add_key_to_view view key) >>= fun res -> 
      dirty := true;
      return ()

    let mem tr key =
      Key_.assert_key key;
      let (store,view,_,_) = tr in
      GitWorkdirIntf.mem store (add_key_to_view view key)

    let tr_key tr =
      let (_,_,key,_) = tr in
      key

  end

type irmin_trans = IrminIntf.store * View.t * Key_.t * bool ref

module IrminIntf_tr : GitIntf_tr with type transaction = irmin_trans =
  struct
    type transaction = irmin_trans

    let begin_transaction ?user config key =
      Key_.assert_key key;
      IrminIntf.create ?user config >>= fun store ->
      IrminIntf.read_view store key >>= fun view ->
      return (store,view,key,ref false)

    let end_transaction tr =
      let (store,view,key,dirty) = tr in
      if !dirty = true then (
        IrminIntf.update_view store key view >>= fun () ->
        dirty := false;
        return ()
      ) else
        return ()

    let remove_view tr =
      let (store,_,key,_) = tr in
      IrminIntf.remove store key

    let move_view tr key2 =
      let (store,view,_,_) = tr in
      IrminIntf.update_view store key2 view 

    let update tr key data =
      Key_.assert_key key;
      let (_,view,_,dirty) = tr in
      View.update view key data >>= fun () ->
      dirty := true;
      return ()

    let read tr key =
      Key_.assert_key key;
      let (_,view,_,_) = tr in
      View.read view key

    let read_exn tr key =
      Key_.assert_key key;
      let (_,view,_,_) = tr in
      View.read_exn view key

    let list tr key =
      Key_.assert_key key;
      let (_,view,_,_) = tr in
      View.list view key

    let remove tr key =
      Key_.assert_key key;
      let (_,view,_,dirty) = tr in
      View.remove_rec view key >>= fun () ->
      dirty := true;
      return ()

    let mem tr key =
      Key_.assert_key key;
      let (_,view,_,_) = tr in
      View.mem view key

    let tr_key tr =
      let (_,_,key,_) = tr in
      key

  end

module type GitMailboxIntf = 
  sig
    type t
    val create : Server_config.imapConfig -> string -> string -> Ssl_.keys -> t Lwt.t
    val commit : t -> unit Lwt.t
    val exists : t -> [`No|`Folder|`Mailbox] Lwt.t
    val create_mailbox : t -> unit Lwt.t
    val delete_mailbox : t -> unit Lwt.t
    val move_mailbox : t -> string -> unit Lwt.t
    val copy_mailbox : t -> [`Sequence of int|`UID of int] -> t ->
      mailbox_message_metadata -> unit Lwt.t
    val read_mailbox_metadata : t -> mailbox_metadata Lwt.t
    val append_message : t -> string -> mailbox_message_metadata -> unit Lwt.t
    val update_mailbox_metadata : t -> mailbox_metadata -> unit Lwt.t
    val update_message_metadata : t -> [`Sequence of int|`UID of int] -> mailbox_message_metadata ->
      [`NotFound|`Eof|`Ok] Lwt.t
    val read_message : t -> ?filter:(searchKey) searchKeys -> 
      [`Sequence of int|`UID of int] ->
      [`NotFound|`Eof|`Ok of (module Lazy_message.LazyMessage_inst)] Lwt.t
    val read_message_metadata : t -> [`Sequence of int|`UID of int] -> 
      [`NotFound|`Eof|`Ok of mailbox_message_metadata] Lwt.t
    val delete_message : t -> [`Sequence of int|`UID of int] -> unit Lwt.t
    val list : t -> subscribed:bool -> ?access:(string -> bool) -> init:'a -> 
      f:('a -> [`Folder of string*int|`Mailbox of string*int] -> 'a Lwt.t) -> 'a Lwt.t
    val read_index_uid : t -> (string*int) list Lwt.t
    val show_all : t -> unit Lwt.t
    val uid_to_seq : t -> int -> int option Lwt.t
    val subscribe : t -> unit Lwt.t
    val unsubscribe : t -> unit Lwt.t
    val create_account : t -> [`Ok|`Exists] Lwt.t
    val delete_account : t -> unit Lwt.t
  end

module GitMailboxMake 
  (GI:GitIntf) 
  (GI_tr:GitIntf_tr) : GitMailboxIntf =
  struct 
    (* user * mailbox * is-folder * irmin key including the mailbox *)
    type t = {user:string;mailbox:string;trans:GI_tr.transaction;
  index:(string*int) list option ref;config: Server_config.imapConfig;mbox_key:
    Key_.t;pubpriv: Ssl_.keys}

    let get_key mbox_key = function
    | `Metamailbox -> Key_.add_path mbox_key "metambox"
    | `Index -> Key_.add_path mbox_key "index"
    | `Storage (msg_hash,contid) -> Key_.t_of_path (String.concat "" ["storage/" ; msg_hash ; "/" ; contid]) 
    | `Hashes hash -> Key_.t_of_path (String.concat hash ["storage/" ; "/hashes"])
    | `Metamessage hash -> Key_.add_path mbox_key (String.concat hash ["messages/" ; "/metamsg"])
    | `Blob hash -> Key_.t_of_path ("storage/" ^ hash) 
    | `MetamsgRoot hash -> Key_.add_path mbox_key ("messages/" ^ hash)
    | `Subscriptions -> Key_.t_of_path "subscriptions"

    (* commit should be called explicitly on each created mailbox to have the
     * changes commited to Irmin
     *)
    let create config user path keys =
      (* view starts at /imaplet/user, then mbox_key continues at
       * mailboxes/Foo/mailboxes/Foo1 etc *)
      let (mailbox,mbox_key) = Key_.mailbox_of_path path in
      GI_tr.begin_transaction ~user config (Key_.create_account user) >>= fun trans ->
        return {user;mailbox;trans;index=ref None;config;mbox_key;pubpriv=keys}

    let commit mbox =
      GI_tr.end_transaction mbox.trans

    (* create mailbox metadata and index stores *)
    let create_mailbox mbox =
      let key = get_key mbox.mbox_key `Metamailbox in
      let metadata = empty_mailbox_metadata ~uidvalidity:(new_uidvalidity())()
        ~selectable:true in
      let sexp = sexp_of_mailbox_metadata metadata in
      GI_tr.update mbox.trans key (Sexp.to_string sexp) >>= fun () ->
      let key = get_key mbox.mbox_key `Index in
      let sexp = sexp_of_list (fun i -> Sexp.of_string i) [] in
      GI_tr.update mbox.trans key (Sexp.to_string sexp) 

    let delete_mailbox mbox =
      GI_tr.remove_view mbox.trans

    (* how to make this the transaction? TBD *)
    let move_mailbox mbox path =
      let (_,key2) = Key_.mailbox_of_path path in
      GI_tr.move_view mbox.trans key2 >>
      GI_tr.remove_view mbox.trans

    let read_mailbox_metadata mbox =
      GI_tr.read_exn mbox.trans (get_key mbox.mbox_key `Metamailbox) >>= fun sexp_str ->
      let sexp = Sexp.of_string sexp_str in
      return (mailbox_metadata_of_sexp sexp)

    let update_mailbox_metadata mbox metadata =
      let sexp = sexp_of_mailbox_metadata metadata in
      let key = get_key mbox.mbox_key `Metamailbox in
      GI_tr.update mbox.trans key (Sexp.to_string sexp)

    let exists mbox =
      catch (fun () ->
        read_mailbox_metadata mbox >>= fun metadata ->
        return (if metadata.selectable then `Mailbox else `Folder)
      )
      (fun _ -> return `No)

    let exists_key mbox key =
      catch (fun () ->
        let key = Key_.add_list mbox.mbox_key (Key_.add_path key "meta") in
        GI_tr.read_exn mbox.trans key >>= fun metadata_sexp_str ->
        let metadata_sexp = Sexp.of_string metadata_sexp_str in
        let metadata = mailbox_metadata_of_sexp metadata_sexp in
        return (if metadata.selectable then `Mailbox else `Folder)
      )
      (fun _ -> return `No)

    let find_flag l fl =
      list_find l (fun f -> f = fl)

    let read_index_uid mbox =
      match mbox.!index with
      | None ->
        GI_tr.read_exn mbox.trans (get_key mbox.mbox_key `Index) >>= fun index_sexp_str ->
        let uids = list_of_sexp 
          (fun i -> 
            pair_of_sexp (fun a -> Sexp.to_string a) (fun b -> int_of_string (Sexp.to_string b)) i
          ) (Sexp.of_string index_sexp_str) in
        mbox.index := Some uids;
        return uids
      | Some uids -> return uids

    let update_index_uids mbox uids = 
      mbox.index := Some uids;
      GI_tr.update mbox.trans (get_key mbox.mbox_key `Index) 
        (Sexp.to_string (sexp_of_list (fun i -> 
          sexp_of_pair (fun a -> Sexp.of_string a) (fun b -> Sexp.of_string (string_of_int b)) i
        ) uids ))

    let update_index_uid mbox (hash,uid) =
      read_index_uid mbox >>= fun uids ->
      if List.exists (fun (_,u) -> u = uid) uids then
        raise DuplicateUID
      else (
        (* reverse index *)
        let uids = ((hash,uid) :: uids) in
        update_index_uids mbox uids
      )

    let uid_to_seq mbox uid =
      read_index_uid mbox >>= fun uids ->
      match (list_findi uids (fun i (_,u) -> u = uid)) with
      | None -> return None
      | Some (i,_) -> return (Some ((List.length uids) - i))

    let get_hashes mbox hash =
      GI_tr.read_exn mbox.trans (get_key mbox.mbox_key (`Hashes hash)) >>= fun h ->
      return (hashes_of_sexp (Sexp.of_string h))

    let update_hashes mbox msg_hash attachments =
      let h = {
        attachments;
      } in
      GI_tr.update mbox.trans (get_key mbox.mbox_key (`Hashes msg_hash)) 
        (Sexp.to_string (sexp_of_hashes h)) >>
      return h

    let workdir_update mbox key data =
      GitWorkdirIntf_tr.begin_transaction ~user:mbox.user mbox.config
        (Key_.create_account mbox.user) >>= fun trans ->
      GitWorkdirIntf_tr.update trans key data >>
      GitWorkdirIntf_tr.end_transaction trans
      
    let workdir_read mbox key =
      GitWorkdirIntf_tr.begin_transaction ~user:mbox.user mbox.config
        (Key_.create_account mbox.user) >>= fun trans ->
      GitWorkdirIntf_tr.read_exn trans key
      
    let workdir_remove mbox key =
      GitWorkdirIntf_tr.begin_transaction ~user:mbox.user mbox.config
        (Key_.create_account mbox.user) >>= fun trans ->
      GitWorkdirIntf_tr.remove trans key

    let _update mbox key data =
      (* irmin/hybrid store - the message MIME parts are written into workdir, not git *)
      if mbox.config.data_store = `Irmin && mbox.config.hybrid then 
        workdir_update mbox key data
      else
        GI_tr.update mbox.trans key data

    let _read mbox key =
      if mbox.config.data_store = `Irmin && mbox.config.hybrid then 
        workdir_read mbox key 
      else
        GI_tr.read_exn mbox.trans key

    let _remove mbox key =
      if mbox.config.data_store = `Irmin && mbox.config.hybrid then 
        workdir_remove mbox key 
      else
        GI_tr.remove mbox.trans key

    let append_message mbox message message_metadata =
      begin
      (* single store - the message is broken into postmark, headers, content,
       * attachments with a map referencing MIME parts. since git is
       * content-addressed storage, identical attachments are deduplicated
       *)
      if mbox.config.single_store then (
        let update msg_hash contid data =
          let key = get_key mbox.mbox_key (`Storage (msg_hash,contid)) in
          _update mbox key data
        in
        let (pub,_) = mbox.pubpriv in
        Email_parse.parse pub mbox.config message ~save_message:(fun msg_hash postmark headers content attachments ->
          update_hashes mbox msg_hash attachments >>= fun h ->
          update msg_hash "0" postmark >>
          update msg_hash "1" headers >>
          update msg_hash "2" content
        ) 
        ~save_attachment:(fun msg_hash contid attachment ->
          update msg_hash contid attachment
        )
      (* same as above but all parts are stored in one file, so there is no
       * deduplication for attachments. this in a way is similar to maildir
       * storage 
       *)
      ) else if mbox.config.maildir_parse then (
        Email_parse.message_to_blob mbox.config mbox.pubpriv message >>= fun (msg_hash,message) ->
        let key = get_key mbox.mbox_key (`Blob msg_hash) in
        _update mbox key message >>
        return msg_hash
      (* this is basically a maildir file stored in irmin
       * the only difference is the separate metadata file
       *)
      ) else (
        Email_parse.message_unparsed_to_blob mbox.config mbox.pubpriv message >>= fun (msg_hash,message) ->
        let key = get_key mbox.mbox_key (`Blob msg_hash) in
        _update mbox key message >>
        return msg_hash
      )
      end >>= fun msg_hash ->
      GI_tr.update mbox.trans (get_key mbox.mbox_key (`Metamessage msg_hash)) 
          (Sexp.to_string (sexp_of_mailbox_message_metadata message_metadata)) >>
      update_index_uid mbox (msg_hash,message_metadata.uid)

    let get_uid mbox position = 
      let nth_uid uids n =
        let (h,u) = List.nth uids n in
        u
      in
      read_index_uid mbox >>= fun uids ->
      match position with
      | `Sequence seq -> 
        if seq > List.length uids then
          return `Eof
        else if seq = 0 then
          return `NotFound
        else (
          let (h,u) = List.nth uids ((List.length uids) - seq) in
          return (`Ok (seq,h,u))
        )
      | `UID uid -> 
        match (list_findi uids (fun i (_,u) -> u = uid)) with 
        | None ->
          if uid > nth_uid uids 0 then
            return `Eof
          else
            return `NotFound
        | Some (seq,(hash,uid)) ->
            return (`Ok ((List.length uids) - seq,hash,uid))

    let update_message_metadata mbox position metadata =
      get_uid mbox position >>= function
      | `Eof -> return `Eof
      | `NotFound -> return `NotFound
      | `Ok (_,hash,_) ->
        GI_tr.update mbox.trans (get_key mbox.mbox_key (`Metamessage hash))
          (Sexp.to_string (sexp_of_mailbox_message_metadata metadata)) >>= fun () ->
        return `Ok

    let read_storage mbox msg_hash contid =
      let key = get_key mbox.mbox_key (`Storage (msg_hash,contid)) in
      _read mbox key

    let get_message_metadata mbox hash =
      GI_tr.read_exn mbox.trans (get_key mbox.mbox_key (`Metamessage hash)) >>= fun sexp_str ->
      return (mailbox_message_metadata_of_sexp (Sexp.of_string sexp_str))

    let read_from_blob mbox hash =
      let lazy_read = Lazy.from_fun (fun () -> 
        _read mbox (get_key mbox.mbox_key (`Blob hash))) in
      Email_parse.message_from_blob mbox.config mbox.pubpriv lazy_read 
      (fun postmark headers content attachment ->
        return (`Ok (
          build_lazy_message_inst
            (module LazyIrminMessage)
            ((fun () -> postmark ()),
            (fun () -> headers ()),
            (fun () -> content ()),
            (attachment),
            (fun () ->
              get_message_metadata mbox hash
            ))
          )
        )
      )

    let read_unparsed_from_blob mbox hash =
      let lazy_read = Lazy.from_fun (fun () -> 
        Log_.log `Info1 (Printf.sprintf "lazy read %s\n" hash);
        _read mbox (get_key mbox.mbox_key (`Blob hash)) >>= fun message ->
        Email_parse.message_unparsed_from_blob mbox.config mbox.pubpriv message
      ) in
      let lazy_message = Lazy.from_fun (fun () ->
        Log_.log `Info1 (Printf.sprintf "lazy message %s\n" hash);
        Lazy.force lazy_read >>= fun buffer ->
        let seq = Mailbox.With_seq.of_string buffer in
        return (Utils.option_value_exn (Mailbox.With_seq.fold_message seq
          ~f:(fun _ message -> Some message) ~init:None))) in
      let lazy_metadata = Lazy.from_fun (fun () -> 
        Log_.log `Info1 (Printf.sprintf "lazy metadata %s\n" hash);
        get_message_metadata mbox hash) in
      return (`Ok  (Lazy_message.build_lazy_message_inst (module LazyMaildirMessage) 
        (lazy_read, lazy_message, lazy_metadata)))

    let read_from_single_store mbox hash =
      let (_,priv) = mbox.pubpriv in
        let priv = Utils.option_value_exn ~ex:EmptyPrivateKey priv in
        let lazy_hashes = Lazy.from_fun (fun () -> get_hashes mbox hash) in
        return (`Ok (
          build_lazy_message_inst
            (module LazyIrminMessage)
            ((fun () ->
              read_storage mbox hash "0" >>= fun postmark ->
              Email_parse.do_decrypt priv mbox.config postmark
            ),
            (fun () ->
              read_storage mbox hash "1" >>= fun headers ->
              Email_parse.do_decrypt_headers priv mbox.config headers
            ),
            (fun () ->
              read_storage mbox hash "2" >>= fun content ->
              Email_parse.do_decrypt_content priv mbox.config content
            ),
            (Email_parse.get_decrypt_attachment priv mbox.config (read_storage mbox hash)),
            (fun () ->
              get_message_metadata mbox hash
            ))
          )
        )

    let read_message mbox ?filter position =
      get_uid mbox position >>= function
      | `Eof -> return `Eof
      | `NotFound -> return `NotFound
      | `Ok (seq,hash,uid) -> 
        if mbox.config.single_store then
          read_from_single_store mbox hash
        else if mbox.config.maildir_parse then
          read_from_blob mbox hash
        else
          read_unparsed_from_blob mbox hash

    let read_message_metadata mbox position =
      get_uid mbox position >>= function
      | `Eof -> return `Eof
      | `NotFound -> return `NotFound
      | `Ok (seq,hash,uid) -> 
        get_message_metadata mbox hash >>= fun meta ->
        return (`Ok meta)

    let copy_mailbox mbox1 pos mbox2 message_metadata =
      get_uid mbox1 pos >>= function
      | `Eof -> return ()
      | `NotFound -> return ()
      | `Ok (seq,hash,uid) ->
        GI_tr.update mbox2.trans (get_key mbox2.mbox_key (`Metamessage hash)) 
          (Sexp.to_string (sexp_of_mailbox_message_metadata message_metadata)) >>
        update_index_uid mbox2 (hash,message_metadata.uid)

    let remove_storage mbox msg_hash contid =
      let key = get_key mbox.mbox_key (`Storage (msg_hash,contid)) in
      _remove mbox key

    let delete_message mbox position =
      get_uid mbox position >>= function
      | `Ok (_,hash,uid) -> 
        begin
        if mbox.config.single_store then (
          get_hashes mbox hash >>= fun h ->
          GI_tr.remove mbox.trans (get_key mbox.mbox_key (`Hashes hash)) >>
          remove_storage mbox hash "0" >>
          remove_storage mbox hash "1" >>
          remove_storage mbox hash "2" >>
          let rec delattach i =
            if i < h.attachments then
              remove_storage mbox hash (string_of_int (3 + i)) >>
              delattach (i + 1)
            else
              return ()
          in
          delattach 0
        ) else (
          _remove mbox (get_key mbox.mbox_key (`Blob hash))
        )
        end >>
        GI_tr.remove mbox.trans (get_key mbox.mbox_key (`Metamessage hash)) >>
        GI_tr.remove mbox.trans (get_key mbox.mbox_key (`MetamsgRoot hash)) >>
        read_index_uid mbox >>= fun uids ->
        let uids = List.fold_right (fun (hash,u) uids ->
          if u = uid then
            uids
          else
            (hash,u) :: uids
        ) uids [] in
        update_index_uids mbox uids
      |_ -> return ()

    let read_subscriptions mbox =
      GI_tr.read mbox.trans (get_key mbox.mbox_key `Subscriptions) >>= function
      | Some str -> return (list_of_str_sexp str)
      | None -> return []
      
    let subscribe mbox =
      read_subscriptions mbox >>= fun l ->
      if list_find l (fun i -> i = mbox.mailbox) then 
        return ()
      else
        GI_tr.update mbox.trans (get_key mbox.mbox_key `Subscriptions)
          (str_sexp_of_list (mbox.mailbox :: l))

    let unsubscribe mbox =
      read_subscriptions mbox >>= fun l ->
      let l = List.fold_left (fun acc i -> if i = mbox.mailbox then acc else i :: acc) [] l in
      GI_tr.update mbox.trans (get_key mbox.mbox_key `Subscriptions)
        (str_sexp_of_list l)

    let list_mailbox mbox key =
      (* sub-mailboxes are located under 'mailboxes' parent *)
      GI_tr.list mbox.trans (Key_.add_list mbox.mbox_key (Key_.mailboxes_of_mailbox key))

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
            list_ mbox key subscriptions access acc f >>= fun (acc,cnt) -> 
            f acc (`Mailbox ((Key_.view_key_to_path key), cnt)) 
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
      let (_,key) = Key_.mailbox_of_path "" in
      begin
      if subscribed then (
        read_subscriptions mbox >>= fun sub -> return (Some sub)
      ) else (
        return None
      ) 
      end >>= fun subscriptions ->
      list_ mbox key subscriptions access init f >>= fun (acc,_) ->
      return acc

    let list_messages mbox k =
      let list_subtr store k =
        GI.list store k >>= fun l ->
        return (List.fold_left (fun acc i ->
          acc ^ ":" ^ (List.nth i ((List.length i) - 1))
        ) "" l)
      in
      GI.create ~user:mbox.user mbox.config >>= fun store ->
      GI.list store k >>= fun l ->
      Lwt_list.fold_left_s (fun acc i ->
        let k = Key_.t_of_list ((List.concat [k;i])) in
        list_subtr store k >>= fun s ->
        return (((Key_.key_to_string (Key_.t_of_list i)) ^ ":" ^ s) :: acc)
      ) [] l

    let show_all mbox =
      Printf.printf "---------- mailbox messages\n%!";
      let (_,key) = Key_.mailbox_of_path mbox.mailbox in
      let key = Key_.add_path key "messages" in
      list_messages mbox key >>= fun l ->
      List.iter (fun i -> Printf.printf "%s %!" i) l; Printf.printf "\n%!";
      Printf.printf "---------- mailbox index\n%!";
      read_index_uid mbox >>= fun uids ->
      List.iter (fun (h,u) -> Printf.printf "%s %d %!" h u) uids; Printf.printf "\n%!";
      Printf.printf "---------- mailbox metadata\n%!";
      read_mailbox_metadata mbox >>= fun meta ->
      Printf.printf "%s\n%!" (Sexp.to_string (sexp_of_mailbox_metadata meta));
      Printf.printf "---------- subscriptions\n%!";
      read_subscriptions mbox >>= fun subscr ->
      List.iter (fun i -> Printf.printf "%s %!" i) subscr; Printf.printf "\n%!";
      return ()

    let create_account mbox =
      let key = Key_.create_account mbox.user in
      GI_tr.begin_transaction ~user:mbox.user mbox.config key >>= fun view ->
      GI_tr.mem view (Key_.t_of_path "subscriptions") >>= fun res ->
      if res then
        return `Exists
      else (
        GI_tr.update view (Key_.t_of_path "subscriptions") (str_sexp_of_list []) >>
        GI_tr.end_transaction view >>
        return `Ok
      )

    let delete_account mbox =
      let key = Key_.create_account mbox.user in
      GI.create ~user:mbox.user mbox.config >>= fun store ->
      GI.remove store key

  end

module GitMailbox = GitMailboxMake(IrminIntf)(IrminIntf_tr)

module GitWorkdirMailbox = GitMailboxMake(GitWorkdirIntf)(GitWorkdirIntf_tr)
