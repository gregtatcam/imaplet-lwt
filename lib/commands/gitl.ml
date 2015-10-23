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
open Nocrypto.Hash

module MapStr = Map.Make(String)

let acct_lock_pool = ref MapStr.empty

let pool_mutex = Lwt_mutex.create ()

exception InvalidObject of string
exception InvalidKey
exception InvalidArgument

let gitreadt = ref 0.
let gitcomprt = ref 0.

module Sha :
sig
  type t
  (* sha is a 20 bytes hex string *)
  val of_hex_string : string -> t
  (* sha is binary *)
  val of_hex_binary : string -> t
  (* calculate sha from the content *)
  val of_string : string -> t
  val empty : t
  val prefix : t -> string
  val postfix : t -> string
  val to_string : t -> string
  val to_binary : t -> string
  val equal : t -> t -> bool
  val is_empty : t -> bool
end =
struct
  type t = string
  let of_hex_binary str =
    let n = String.length str in
    let rec get i acc = 
      if i = n then
        acc
      else
        get (i+1) ((Printf.sprintf "%02x" (int_of_char (String.get str i))) :: acc)
    in
    String.concat "" (List.rev (get 0 []))
  let of_hex_string str = str
  let of_string str =
    of_hex_binary (Cstruct.to_string (SHA1.digest (Cstruct.of_string str)))

  let empty = "00000000000000000000000000000000000000"
  let prefix t = String.sub t 0 2
  let postfix t = String.sub t 2 (String.length t - 2)
  let to_string t = t
  let to_binary t =
    let buff = Buffer.create 10 in
    let len = String.length t in
    let rec loop i =
      if i = len then
        Buffer.contents buff
      else (
        Buffer.add_char buff (char_of_int (int_of_string ("0x" ^ (String.sub t i 2))));
        loop (i + 2)
      )
    in
    loop 0
  let equal t t1 =
    (t = t1)
  let is_empty t =
    t = empty
end

module Key :
sig
  type t = string list
  val create : string list -> t
  val of_unix : string -> t
  val to_string : t -> string
  val to_string_rev : t -> string
  val parent : t -> t
  val replace_wild : t -> Sha.t -> t
  val add : t -> string -> t
  val last : t -> string
  val empty : t
  val is_empty : t -> bool
  val equal : t -> t -> bool
end =
struct
  type t = string list
  let re = Re_posix.compile_pat "/"
  let create t = t
  let of_unix str =
    Re.split re str
  let to_string t =
    ("/" ^ String.concat "/" t)
  let to_string_rev t =
    to_string (List.rev t)
  let parent t =
    let (_,key) =
      List.fold_right (fun k (first,acc) ->
        if first then
          (false,acc)
        else
          (false,k :: acc)
      ) t (true,[])
    in
    key
  let replace_wild t sha =
    if List.nth t ((List.length t) - 1) = "*" then
      List.concat [parent t; [Sha.to_string sha]]
    else
      t
  let add t k =
    List.concat [t;of_unix k]
  let last t =
    List.nth t (List.length t - 1)
  let empty = []
  let is_empty t =
    t = []
  let equal t1 t2 =
    t1 = t2
end

type commit =
  {tree:Sha.t;parent:Sha.t list;author:string;committer:string;message:string}
type perm = [`Normal|`Exec|`Link|`Dir|`Commit]
type tree_entry = {perm:perm;name:string;sha:Sha.t}
type tree = tree_entry list
type head = {repo:string;sha:Sha.t}
type obj_type = [`Tree of tree|`Blob of string|`Commit of commit|`Tag of string|`None]
type log =
{parent:Sha.t;commit:Sha.t;author:string;message:string;postfix:string;date:string;utc:string}
type cache_type = ([`Read |`Write] * string * tree) Map.Make(String).t ref
type t = {root:string;head:head;commit:commit;compress:bool;
  cache:cache_type option}

(* cache for tree's 
 * default cache maintains read only access
 * per instance cache lets emulate view-like commits
 * where updates to blobs written to FS but updated tree's
 * are stored in cache and written to FS on commit. This way
 * there is one commit for multiple updates
 *
 * with multiple connections to the same account need to expand
 * the cache logic, having cache objects referenced by the path
 * doesn't provide version information so multiple connections
 * will be overwriting each other's cache. also when objects are in-memory
 * updated the cache is overwritten making access from other connections
 * invalid (update_tree updates all trees in the path, including the root, so
 * the Gitl object itself will reference the 'wrong' tree. TBD
 *)
let default_cache = ref (MapStr.empty)

let clear_default_cache () =
  default_cache := MapStr.empty;;

let cache_exists cache ?sha k =
  let cache = match cache with Some c->c|None->default_cache in
  if MapStr.mem k !cache then (
    Log_.log `Info3 (Printf.sprintf "cache exists %s\n%!" k);
    let (_,_,v) = MapStr.find k !cache in
     Some v
  ) else (
    Log_.log `Info3 (Printf.sprintf "cache doesn't exist %s\n%!" k);
    None
  )

(* read can only update read
 * write can update read and write
 *)
let add_cache cache k v =
  let cache = match cache with Some c->c|None->default_cache in
  let (rw,s,_) = v in
  Log_.log `Info3 (Printf.sprintf "adding cache %s, %s\n%!" k s);
  if MapStr.mem k !cache then ( 
    let (_rw,_,_) = MapStr.find k !cache in
    if rw = `Write || rw = _rw then
      cache := MapStr.add k v !cache
  ) else (
    cache := MapStr.add k v !cache
  )

let rem_cache cache k =
  if MapStr.mem k !cache then
    cache := MapStr.remove k !cache

let obj_type_to_string = function
  | `Blob _ -> "blob"
  | `Tree _ -> "tree"
  | `Commit _ -> "commit"
  | `Tag _ -> "tag"
  | `None -> "none"

let raise_inv_obj msg = function
  | `Blob _ -> raise (InvalidObject (msg ^ "blob"))
  | `Commit _ -> raise (InvalidObject (msg ^ "commit"))
  | `Tree _ -> raise (InvalidObject (msg ^ "tree"))
  | `Tag _ -> raise (InvalidObject (msg ^ "tag"))
  | `None -> raise (InvalidObject (msg ^ "none"))

let file_exists ?(t=[Unix.S_REG]) file =
  catch (fun () ->
  Lwt_unix.stat file >>= fun st ->
  return (List.exists (fun k -> st.Unix.st_kind = k) t))
  (fun _ -> return false)

let fconcat dir l =
  List.fold_left (fun acc f -> Filename.concat acc f) dir l

let read_file file =
  let open Unix in
  Lwt_io.with_file ~flags:[O_NONBLOCK] ~mode:Lwt_io.input file Lwt_io.read

let preemptive ?(preempt=false) f =
  if preempt then 
    Lwt_preemptive.detach (fun () -> f()) ()
  else 
    return (f())

let read_file_inflate ?(preempt=false) file =
  let t = Unix.gettimeofday () in
  read_file file >>= fun content ->
  let t1 = Unix.gettimeofday () in
  gitreadt := !gitreadt +. (t1 -. t);
  let uncompress () = Imap_crypto.do_uncompress ~header:true content in
  preemptive ~preempt uncompress >>= fun content ->
  gitcomprt := !gitcomprt +. (Unix.gettimeofday() -. t1);
  return content

let read_ ?(compress=false) ?(preempt=false) file =
  if compress then
    read_file_inflate ~preempt file
  else
    read_file file

let write_file ?(append=false) file content=
  let open Unix in
  let flags =
    if append then
      [O_NONBLOCK;O_WRONLY;O_CREAT;O_APPEND]
    else
      [O_NONBLOCK;O_WRONLY;O_CREAT;O_TRUNC]
  in
  Lwt_io.with_file ~flags ~mode:Lwt_io.Output file (fun oc -> Lwt_io.write oc content)

let write_file_deflate ?(append=false) ?(preempt=false) file content =
  let compress ()= Imap_crypto.do_compress ~header:true content in
  preemptive ~preempt compress >>= fun content ->
  write_file ~append file content

let write_ ?(append=false) ?(preempt=false) ?(compress=false) ~file content =
  if compress then
    write_file_deflate ~append ~preempt file content
  else
    write_file ~append file content

module Commit :
sig
  type t = commit
  (* parses commit objects from a string buffer *)
  val create : string -> t Lwt.t
  (* create commit from components *)
  val of_values : tree:Sha.t -> parent:Sha.t list -> author:string ->
    committer:string -> message:string -> t
  val to_string : t -> string
  val to_object_string : t -> string
  val empty : t
end =
struct
  type t = commit
  let re = Re_posix.compile_pat "^(tree|parent|author|committer) ([^ ]+)$"
  let empty = {tree=Sha.empty;parent=[];author="";committer="";message=""}
  let create content =
    let ic = Lwt_io.of_bytes ~mode:Lwt_io.Input (Lwt_bytes.of_string content) in
    let message = Buffer.create 100 in
    let rec get t data =
      Lwt_io.read_line_opt ic >>= function 
      | Some line ->
        if data then (
          Buffer.add_string message line;
          Buffer.add_string message "\n";
          return t
        ) else (
          try
            let subs = Re.exec re line in
            match Re.get subs 1 with
            | "tree" -> get {t with tree = Sha.of_hex_string (Re.get subs 2)} data
            | "parent" -> get {t with parent = Sha.of_hex_string ((Re.get subs 2)) :: t.parent} data
            | "author" -> get {t with author = (Re.get subs 2)} data
            | "committer" -> get {t with committer = (Re.get subs 2)} data
            | x -> raise (InvalidObject x)
          with Not_found -> Buffer.add_string message line; get t true
        )
      | None ->
        if Buffer.length message > 0 then 
          return {t with message = Buffer.contents message}
        else
          return t
    in
    get empty false

  let of_values ~tree ~parent ~author ~committer ~message =
    {tree;parent;author;committer;message}

  let parent_to_string p =
    let str = String.concat "\n" (List.map 
      (fun p -> Printf.sprintf "parent %s" (Sha.to_string p)) 
        (List.filter (fun p -> Sha.is_empty p = false) p)) in
    if str = "" then
      "parent"
    else
      str

  let to_string commit =
    Printf.sprintf "tree: %s\n%s\nauthor: %s\ncommitter: %s\nmessage: %s\n"
      (Sha.to_string commit.tree) (parent_to_string commit.parent) 
      commit.author commit.committer commit.message

  let to_object_string commit =
    Printf.sprintf "tree %s\n%s\nauthor %s\ncommitter %s\n\n%s"
      (Sha.to_string commit.tree) (parent_to_string commit.parent)
      commit.author commit.committer commit.message
end

module Tree :
sig
  type t = tree
  val create : string -> t
  val to_string : t -> string
  val to_object_string : t -> string
  val update_tree_entry :t -> string -> perm -> Sha.t -> t
  val delete_tree_entry :t -> string -> t
  val rename_tree_entry :t -> src:string -> dest:string -> t
  val empty : t
end =
struct
  type t = tree
  let perm_of_string = function
  | "44"
  | "100644" -> `Normal
  | "100755" -> `Exec
  | "120000" -> `Link
  | "40000"  -> `Dir
  | "160000" -> `Commit
  | x        -> raise (InvalidObject x)

  let string_of_perm = function
  | `Normal -> "100644"
  | `Exec   -> "100755"
  | `Link   -> "120000"
  | `Dir    -> "40000"
  | `Commit -> "160000"

  let fixed_length_string_of_perm = function
  | `Normal -> "100644"
  | `Exec   -> "100755"
  | `Link   -> "120000"
  | `Dir    -> "040000"
  | `Commit -> "160000"

  let perm_to_obj_type = function
  | `Normal -> `Blob
  | `Exec   -> `Blob
  | `Link   -> `Blob
  | `Dir    -> `Tree
  | `Commit -> `Commit

  let perm_to_type_string = function
  | `Normal -> "blob"
  | `Exec   -> "blob"
  | `Link   -> "blob"
  | `Dir    -> "tree"
  | `Commit -> "commit"

  let re = Re_posix.compile_pat "([0-9]+) ([^\000]+)\000"

  let empty = []

  let trsort te te1 =
    String.compare te.name te1.name

  let create content =
    let len = String.length content in
    let rec get pos entries =
      if pos >= len then
        entries
      else (
        let subs = Re.exec ~pos re content in
        let perm = Re.get subs 1 in
        let name = Re.get subs 2 in
        let pos = pos + (String.length perm) + (String.length name) + 2 in
        let sha = Sha.of_hex_binary (String.sub content pos 20) in
        get (pos + 20) ({perm=perm_of_string perm;name;sha} :: entries)
      )
    in
    List.sort trsort (List.rev (get 0 []))

  let delete_tree_entry t name =
    List.filter (fun te -> te.name <> name) t

  let update_tree_entry t name perm sha =
    let t = delete_tree_entry t name in
    List.sort trsort ({name;perm;sha} :: t)

  let rename_tree_entry t ~src ~dest =
    if List.exists (fun te -> te.name = src) t = false then raise InvalidArgument;
    if List.exists (fun te -> te.name = dest) t then raise InvalidArgument;
    List.sort trsort (List.map (fun te -> if te.name = src then {te with name=dest} else te) t)

  let to_string tree =
    String.concat "\n" (List.map (fun e -> 
      Printf.sprintf "%s %s %s %s" (string_of_perm e.perm) 
        (perm_to_type_string e.perm) e.name (Sha.to_string e.sha)) tree)

  let to_object_string tree =
    String.concat "" ((List.map (fun e ->
      Printf.sprintf "%s %s\000%s" (string_of_perm e.perm) e.name 
        (Sha.to_binary e.sha)
    )) tree)
end

module Object :
sig
  type t
  val create : ?compress:bool -> string -> t
  val read : t -> Sha.t -> obj_type Lwt.t
  val write : t -> [`Blob of string|`Tree of Tree.t|
    `Commit of Commit.t|`Tag of string] -> Sha.t Lwt.t
  val blob_sha : string -> Sha.t
  val tree_sha : tree -> Sha.t
  val exists : t -> Sha.t -> bool Lwt.t
end =
struct
  type t = {root:string;compress:bool}

  let create ?(compress=false) root = 
    {root;compress}

  let file_from_sha t sha =
    fconcat t.root ["objects"; Sha.prefix sha; Sha.postfix sha]

(* read object type/content *)
  let read t sha =
    if Sha.is_empty sha then
      return `None
    else
    catch (fun () ->
      let file = file_from_sha t sha in
      read_ ~compress:t.compress file >>= fun obj ->
      let subs = Re.exec 
        (Re_posix.compile_pat "^(blob|tree|commit|tag) ([0-9]+)\000") obj in
      let obj_type = Re.get subs 1 in
      let size = int_of_string (Re.get subs 2) in
      let (_,offset) = Re.get_ofs subs 2 in
      let content = String.sub obj (offset+1) size in
      match obj_type with 
      | "blob" -> return (`Blob content)
      | "tree" -> return (`Tree (Tree.create content))
      | "commit" -> 
        Commit.create content >>= fun commit ->
        return (`Commit commit)
      | "tag" -> return (`Tag content)
      | x -> raise (InvalidObject x)
    ) (function
        | Unix.Unix_error (e,f,a) when e = Unix.ENOENT -> return `None
        | ex -> Printf.printf "exception read sha: %s %s:\n%!" (Sha.to_string sha) 
          (Printexc.to_string ex); return `None)

  let make_obj obj =
    match obj with
    | `Blob content -> 
      Printf.sprintf "blob %d\000%s" (String.length content) content
    | `Commit commit -> 
      let str = Commit.to_object_string commit in
      Printf.sprintf "commit %d\000%s" (String.length str) str
    | `Tree tree ->
      let str = Tree.to_object_string tree in
      Printf.sprintf "tree %d\000%s" (String.length str) str
    | `Tag content ->
      Printf.sprintf "tag %d\000%s" (String.length content) content

  let obj_sha obj =
    Sha.of_string (make_obj obj)

  let blob_sha content = obj_sha (`Blob content)

  let tree_sha t = obj_sha (`Tree t)

  let write t obj =
    let content = make_obj obj in
    let sha = Sha.of_string content in
    let file = file_from_sha t sha in
    catch (fun () ->
      write_ ~compress:t.compress ~file content
    ) (function |Unix.Unix_error (e,f,a) when e = Unix.ENOENT ->
      let dir = Filename.dirname file in
      Lwt_unix.mkdir dir 0o751 >>= fun () ->
      write_ ~compress:t.compress ~file content
      | e -> raise e
    ) >>= fun () ->
    return sha

  let exists t sha =
    let file = file_from_sha t sha in
    file_exists ~t:[Unix.S_REG] file
end

module Head :
sig
  type t = head

  val create : string -> t Lwt.t
  val update : t -> Sha.t -> t Lwt.t
  val empty : string -> t
end =
struct
  type t = head

  (* get head's SHA *)
  let read_ repo =
    let head = Filename.concat repo ".git/HEAD" in
    read_file head >>= fun refs ->
    (* ref: refs/heads/master *)
    let subs = Re.exec (Re_posix.compile_pat "^ref: (.+)(\n)?$") refs in
    catch (fun () ->
      read_file (fconcat repo [".git"; (Re.get subs 1)]) >>= fun head ->
      return (Sha.of_hex_string (String.trim head))
    ) (function |Unix.Unix_error (e,f,a) when e = Unix.ENOENT -> return Sha.empty | ex -> raise ex)

  let read t = 
    read_ t.repo

  let update t sha =
    let file = Filename.concat t.repo ".git/refs/heads/master" in
    write_file file (Sha.to_string sha) >>= fun () ->
    return {t with sha}

  let create repo = 
    read_ repo >>= fun sha ->
    return {repo;sha}

  let empty repo =
    {repo;sha=Sha.empty}
end

module Log :
sig
  type t
  val create : string -> t
  val read : t -> log list Lwt.t
  val update : t -> log -> unit Lwt.t
  val to_string : log list -> string list
  val to_log_string : log list -> string list
end =
struct
  type t = string

  let create repo = repo

  let read t =
    catch (fun () ->
    let file = fconcat t ["logs/refs/heads/master"] in
    Lwt_stream.to_list (Lwt_io.lines_of_file file) >>= fun l ->
    return (
      let re = Re_posix.compile_pat 
        "^([^ ]+) ([^ ]+) (.+) ([0-9]+) ([+-][0-9]+)[ \t]+commit(.+)?: (.+)+$" in
      List.map (fun str ->
        let subs = Re.exec re str in
        let parent = Sha.of_hex_string (Re.get subs 1) in
        let commit = Sha.of_hex_string (Re.get subs 2) in
        let author = Re.get subs 3 in
        let date = Re.get subs 4 in
        let utc = Re.get subs 5 in
        let postfix = try Re.get subs 6 with Not_found -> "" in
        let message = Re.get subs 7 in
        {parent;commit;author;message;postfix;date;utc}
      ) l
    )
    ) (function |Unix.Unix_error (e,f,a) when e = Unix.ENOENT -> return [] | ex -> 
      Printf.printf "exception read log: %s\n%!" (Printexc.to_string ex); raise ex)

  let to_string l =
    let open Unix in
    List.map (fun (log:log) ->
    (* Sat Oct 10 01:54:24 2015 *)
    let tm = Unix.gmtime (float_of_string log.date) in
    let date = Printf.sprintf "%s %s %d %d:%d:%d %d" 
      (List.nth ["Sun";"Mon";"Tue";"Wed";"Thu";"Fri";"Sat"] tm.tm_wday)
      (List.nth ["Jan";"Feb";"Mar";"Apr";"May";"Jun";"Jul";"Aug";"Sep";"Oct";"Nov";"Dec"] tm.tm_mon)
      tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec (1900+tm.tm_year) in
    Printf.sprintf "commit %s\nAuthor: %s\nDate: %s %s\n\n%s\n"
      (Sha.to_string log.commit) log.author date
      log.utc log.message
    ) l

  let to_log_format (log:log) =
    Printf.sprintf "%s %s %s %s %s\tcommit%s: %s\n"
      (Sha.to_string log.parent) (Sha.to_string log.commit) log.author log.date
      log.utc log.postfix log.message 

  let to_log_string log =
    List.map to_log_format log

  let update t log =
    let file = fconcat t ["logs/refs/heads/master"] in
    let str = to_log_format log in
    write_file ~append:true file str
end

let read_object t sha =
  let obj = Object.create ~compress:t.compress t.root in
  Object.read obj sha

let write_object t obj =
  let t = Object.create ~compress:t.compress t.root in
  Object.write t obj

let init root =
  file_exists ~t:[Unix.S_DIR] root >>= fun res ->
  if res = false then (
    Lwt_unix.mkdir root 0o775 >>= fun () ->
    Lwt_unix.mkdir (fconcat root ["hooks"]) 0o751 >>= fun () ->
    Lwt_unix.mkdir (fconcat root ["info"]) 0o751 >>= fun () ->
    Lwt_unix.mkdir (fconcat root ["objects"]) 0o751 >>= fun () ->
    Lwt_unix.mkdir (fconcat root ["objects";"info"]) 0o751 >>= fun () ->
    Lwt_unix.mkdir (fconcat root ["objects";"pack"]) 0o751 >>= fun () ->
    Lwt_unix.mkdir (fconcat root ["refs"]) 0o755 >>= fun () ->
    Lwt_unix.mkdir (fconcat root ["refs";"heads"]) 0o751 >>= fun () ->
    Lwt_unix.mkdir (fconcat root ["refs";"tags"]) 0o751 >>= fun () ->
    Lwt_unix.mkdir (fconcat root ["logs"]) 0o751 >>= fun () ->
    Lwt_unix.mkdir (fconcat root ["logs";"refs"]) 0o751 >>= fun () ->
    Lwt_unix.mkdir (fconcat root ["logs";"refs";"heads"]) 0o751 >>= fun () ->
    write_file (Filename.concat root "config")
    (String.concat "\n\t\t" [
      "[core]"; 
      "repositoryformatversion = 0"; 
      "filemode = true";
      "bare = true";
    ]) >>= fun () ->
    write_file (fconcat root ["HEAD"]) "ref: refs/heads/master" >>= fun () ->
    write_file (fconcat root ["description"]) "Unnamed repository; edit \
      this file 'description' to name the repository." >>= fun () ->
    return true
  ) else 
    return false

(* if cache is provided then updated tree objects are written to cache, commit
 * has to be explicitly called to write the objects to the FS
 * should only write updated tree (i.e. read but not updated should not be
 * written), also need sha of the tree so that it is not calcuted again
 * TBD
 *)
let create ?cache ?(compress=false) ~repo () = 
  begin
  match cache with 
  | Some c ->
    MapStr.iter (fun k v -> c := MapStr.add k v !c) !default_cache
  | None -> ()
  end;
  let root = Filename.concat repo ".git" in
  init root >>= fun inited ->
  if inited then (
    return {root;head=Head.empty repo;commit=Commit.empty;compress;cache}
  ) else (
    let lock = Imap_lock.create pool_mutex acct_lock_pool in
    Imap_lock.with_lock lock root (fun () ->
      Head.create repo) >>= fun head ->
    if Sha.is_empty head.sha then (
      return {root;head;commit=Commit.empty;compress;cache}
    ) else (
      let o = Object.create ~compress root in
      Object.read o head.sha >>= function
      | `Commit commit ->
        return {root;head;commit;compress;cache}
      | x -> 
        raise_inv_obj "create " x
    )
  )

(* find tree/blob's sha corresponding to the key 
 * traverse the tree starting with the root
 * search for child in parent, try the cache first.
 * if found then go to the next level down
 * add to cache if needed
 * if not found then raise
 *)
let find_sha_opt t key = 
  (* start with the root *)
  let parent = Key.empty in
  let sha = t.commit.tree in 
  catch (fun () ->
    Lwt_list.fold_left_s (fun (parent,sha) child ->
      let parent_path = Key.to_string_rev parent in (* parent is built in reverse order *)
      match cache_exists t.cache ~sha:(Sha.to_string sha) parent_path with
      | Some obj's_tree ->
        let te = List.find (fun te -> te.name = child) obj's_tree in
        return (child::parent,te.sha)
      | None when Sha.is_empty sha ->
        raise Not_found
      | _ ->
        read_object t sha >>= fun obj ->
        match obj with
        | `Tree tr -> (* only tree could be in the path *)
          let te = List.find (fun te -> te.name = child) tr in
          add_cache t.cache parent_path (`Read,Sha.to_string sha,tr);
          return (child::parent,te.sha)
        | x -> raise_inv_obj "find_sha_opt " x
    ) (parent,sha) key >>= fun (_,sha) ->
    return (Some sha)
  ) (function |Not_found -> return None|ex -> raise ex)

let find_sha_exn t key =
  find_sha_opt t key >>= function
  | Some sha -> return sha
  | None -> raise InvalidKey

(* find tree/blob's object corresponding to the key *)
let find_obj_opt t key = 
  catch (fun () ->
    (* check the cache first 
     *)
    match cache_exists t.cache (Key.to_string key) with
    | Some obj's_tree -> return (`Tree obj's_tree)
    | None ->
      find_sha_opt t key >>= function
      | Some sha ->
        begin
        read_object t sha >>= function
        | `Blob c -> return (`Blob c)
        | `Tree tr -> 
          add_cache t.cache (Key.to_string key) (`Read,Sha.to_string sha,tr);
          return (`Tree tr)
        | _ -> return `None
        end
    | None -> return `None
  ) (function |Not_found -> return `None|ex -> raise ex)

let read_opt t key = 
  find_obj_opt t key >>= function
  | `Blob c -> return (Some c)
  | _ -> return None

let read_exn t key = 
  read_opt t key >>= function
  | Some v -> return v
  | None -> raise Not_found

(* does the key exist *)
let mem t key =
  find_sha_opt t key >>= function
  | Some _ -> return true
  | None -> return false

(* update/create trees in the path 
 * walk backward to build Merkle tree with updated sha's
 * if deleting/renaming then all sub-trees in the path must exist
 * only leaf needs to be added/deleted, the rest are updates
 *)
let update_tree t key op =
  let obj = Object.create ~compress:t.compress t.root in
  let update_tree_object parent tr op =
    let tr =
    match op with
    | `Add (name,perm,sha) -> Tree.update_tree_entry tr name perm sha
    | `Rename (name,dest) -> Tree.rename_tree_entry tr ~src:name ~dest
    | `Delete name -> Tree.delete_tree_entry tr name
    in
    if t.cache = None then (
      Object.write obj (`Tree tr)
    ) else (
      let sha = Object.tree_sha tr in
      add_cache t.cache (Key.to_string parent) (`Write,Sha.to_string sha,tr);
      return sha
    )
  in
  Lwt_list.fold_right_s (fun child (parent,op) ->
    find_obj_opt t parent >>= function
    | `Tree tr -> update_tree_object parent tr op >>= fun sha ->
      return (Key.parent parent,`Add (child,`Dir,sha)) (* only leaf could be
        deleted/added, others add only *)
    | `None -> update_tree_object parent Tree.empty op >>= fun sha ->
      return (Key.parent parent,`Add (child,`Dir,sha))
    | x -> raise_inv_obj "update_ " x
  ) key (key,op) >>= fun (root,op) ->
  find_obj_opt t root >>= function (* update root *)
  | `Tree tr -> update_tree_object root tr op
  | _ -> update_tree_object root Tree.empty op

(* update/create value in key, write out updated tree,
 * returns sha of updated object and the root's tree sha
 * ASSUME ONE WRITER FOR NOW TBD
 *)
let update_ t key v =
  Log_.log `Info3 (Printf.sprintf "updating %s, %d\n%!" (Key.to_string key) (String.length v));
  if Key.is_empty key then
    raise InvalidArgument;
  let obj = Object.create ~compress:t.compress t.root in
  Object.write obj (`Blob v) >>= fun v'sha ->
  let child = if Key.last key = "*" then Sha.to_string v'sha else Key.last key in
  update_tree t (Key.parent key) (`Add (child,`Normal,v'sha)) >>= fun sha ->
  return (v'sha,sha)

let remove t key =
  if Key.is_empty key then
    raise InvalidArgument;
  update_tree t (Key.parent key) (`Delete (Key.last key))

let rename t ~src ~dest =
  if Key.is_empty src || Key.is_empty dest || Key.equal src dest then
    raise InvalidArgument;
  find_obj_opt t (Key.parent src) >>= function 
  | `Tree tr ->
    let te = List.find (fun te -> te.name = (Key.last src)) tr in
    update_tree t (Key.parent dest) (`Add (Key.last dest,te.perm,te.sha))
  | _ -> raise Not_found

let commit t sha ~author ~message =
 (* to support multiple writers need to lock the HEAD, compare this commit 
  * with the latest commit, if different then load the latest commit, merge,
  * write new commit with update parent, update HEAD, unlock HEAD TBD
  *)
  Log_.log `Info3 "### Committing\n%!";
  if author = "" || message = "" then
    raise InvalidArgument;
  (* multiple parents? *)
  let commit = Commit.of_values ~tree:sha ~parent:[t.head.sha] ~author:""
      ~committer:"" ~message:"" in
  let obj = Object.create ~compress:t.compress t.root in
  begin
  match t.cache with
  | Some cache ->
    let tocommit = MapStr.fold (fun path (rw,sha,tr) acc -> 
      if rw = `Write then (
        (path,sha,tr)::acc 
      ) else 
        acc
    ) !cache [] in
    Lwt_list.iter_p (fun (path,sha,tr) -> 
      add_cache t.cache path (`Read,sha,tr);
      add_cache None path (`Read,sha,tr);
      Object.write obj (`Tree tr) >>= fun _ -> return ()) tocommit
  | None -> return ()
  end >>= fun () ->
  Object.write obj (`Commit commit) >>= fun sha ->
  let lock = Imap_lock.create pool_mutex acct_lock_pool in
  Imap_lock.with_lock lock t.root (fun () ->
    Head.update t.head sha ) >>= fun head ->
  let log = Log.create t.root in
  let date = Printf.sprintf "%d" (int_of_float (Unix.time ())) in
  Log.update log
    {parent=t.head.sha;commit=sha;author;message;postfix="";date;utc="+0000"} >>= fun () ->
  return {t with head;commit}

let update t key v =
  update_ t key v

let update_with_commit t ~author ~message key v =
  if author = "" || message = "" then
    raise InvalidArgument;
  update t key v >>= fun (v'sha,sha) ->
  commit t sha author message >>= fun t ->
  return (v'sha,t)

let remove_with_commit t ~author ~message key v =
  if author = "" || message = "" then
    raise InvalidArgument;
  remove t key >>= fun sha ->
  commit t sha author message

let rename_with_commit t ~author ~message ~src ~dest v =
  if author = "" || message = "" then
    raise InvalidArgument;
  rename t ~src ~dest >>= fun sha ->
  commit t sha author message

let to_string t =
  Printf.sprintf "head: %s\n%s\n" (Sha.to_string t.head.sha) (Commit.to_string t.commit)

let pretty_log t =
  let log = Log.create t.root in
  Log.read log >>= fun l ->
  return (Log.to_string l)

let list t ?(filter_blob=true) key =
  find_obj_opt t key >>= function
  | `Blob _ -> if filter_blob then return [] else return [key]
  | `Tree tree ->
    return (List.fold_right (fun te acc -> 
      if filter_blob && te.perm = `Dir then 
        (Key.add key te.name) :: acc
      else
        acc
      ) tree [])
  | x -> raise_inv_obj "list " x
