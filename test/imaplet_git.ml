open Lwt

module MapStr = Map.Make(String)

exception InvalidObject

let gitreadt = ref 0.
let gitcomprt = ref 0.

module Key :
sig
  type t = string list
  val create : string list -> t
  val of_unix : string -> t
  val to_string : t -> string
  val to_string_rev : t -> string
end =
struct
  type t = string list
  let re = Re_posix.compile_pat "/"
  let create t = t
  let of_unix str =
    Re.split re str
  let to_string t =
    String.concat "/" t
  let to_string_rev t =
    to_string (List.rev t)
end

module Sha :
sig
  type t
  val of_string : string -> t
  val of_raw : string -> t
  val empty : t
  val prefix : t -> string
  val postfix : t -> string
  val to_string : t -> string
end =
struct
  type t = string
  let of_raw str =
    let n = String.length str in
    let rec get i acc = 
      if i = n then
        acc
      else
        get (i+1) ((Printf.sprintf "%02x" (int_of_char (String.get str i))) :: acc)
    in
    String.concat "" (List.rev (get 0 []))
  let of_string str = str
  let empty = "00000000000000000000000000000000000000"
  let prefix t = String.sub t 0 2
  let postfix t = String.sub t 2 (String.length t - 2)
  let to_string t = t
end

type commit =
  {tree:Sha.t;parent:Sha.t list;author:string;committer:string;message:string}
type perm = [`Normal|`Exec|`Link|`Dir|`Commit]
type obj_type = [`Tree|`Blob|`Commit|`Tag]
type tree_entry = {perm:perm;name:string;sha:Sha.t}
type tree = tree_entry list
type t = {root:string;head:Sha.t;commit:commit}

module Commit :
sig
  type t = commit
  val create : string -> t Lwt.t
  val to_string : t -> string
end =
struct
  type t = commit
  let re = Re_posix.compile_pat "^(tree|parent|author|committer) ([^ ]+)$"
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
            | "tree" -> get {t with tree = Sha.of_string (Re.get subs 2)} data
            | "parent" -> get {t with parent = Sha.of_string ((Re.get subs 2)) :: t.parent} data
            | "author" -> get {t with author = (Re.get subs 2)} data
            | "committer" -> get {t with committer = (Re.get subs 2)} data
            | _ -> raise InvalidObject
          with Not_found -> Buffer.add_string message line; get t true
        )
      | None ->
        if Buffer.length message > 0 then 
          return {t with message = Buffer.contents message}
        else
          return t
    in
    get {tree=Sha.empty;parent=[];author="";committer="";message=""} false

  let to_string commit =
    let parent = String.concat "\n" (List.map 
      (fun p -> Printf.sprintf "parent %s" (Sha.to_string p)) commit.parent) in
    Printf.sprintf "tree: %s\n%s\nauthor: %s\ncommitter: %s\nmessage: %s\n"
      (Sha.to_string commit.tree) parent commit.author commit.committer commit.message
end

module Tree :
sig
  type t = tree
  val create : string -> t
  val to_string : t -> string
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
  | x        -> raise InvalidObject

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
        let sha = Sha.of_raw (String.sub content pos 20) in
        get (pos + 20) ({perm=perm_of_string perm;name;sha} :: entries)
      )
    in
    List.rev (get 0 [])

  let to_string tree =
    String.concat "\n" (List.map (fun e -> 
      Printf.sprintf "%s %s %s %s" (string_of_perm e.perm) 
        (perm_to_type_string e.perm) e.name (Sha.to_string e.sha)) tree)
end

let read_file file =
  Lwt_io.with_file ~flags:[Unix.O_NONBLOCK] ~mode:Lwt_io.Input file (fun ic ->
    Lwt_io.read ic)

let read_file_inflate file =
  let t = Unix.gettimeofday () in
  read_file file >>= fun content ->
  let t1 = Unix.gettimeofday () in
  gitreadt := !gitreadt +. (t1 -. t);
  (* uncompress blocks, offload to preemptive thread *)
  (*Lwt_preemptive.detach (fun() -> Compress.do_uncompress ~header:true content)
   * () >>= fun content ->*)
  Compress.do_uncompress ~header:true content >>= fun content ->
  gitcomprt := !gitcomprt +. (Unix.gettimeofday() -. t1);
  return content

(* get head's SHA *)
let read_head repo =
  let head = Filename.concat repo ".git/HEAD" in
  read_file head >>= fun refs ->
  (* ref: refs/heads/master *)
  let subs = Re.exec (Re_posix.compile_pat "^ref: (.+)\n$") refs in
  read_file (Filename.concat repo (Filename.concat ".git" (Re.get subs 1))) >>= fun head ->
  return (Sha.of_string head)

let file_from_sha root sha =
  Filename.concat root (
    Filename.concat "objects" (
      Filename.concat 
        (Sha.prefix sha) (Sha.postfix sha)
    )
  )

(* read object type/content *)
let _read_object root sha =
  let file = file_from_sha root sha in
  read_file_inflate file >>= fun obj ->
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
  | _ -> raise InvalidObject

let read_object t sha =
  _read_object t.root sha

let create ~repo = 
  let root = Filename.concat repo ".git" in
  read_head repo >>= fun head ->
  _read_object root head >>= function
  | `Commit commit ->
    return {root;head;commit}
  | _ -> raise InvalidObject

let search_cache = ref (MapStr.empty)

let clear_search_cache () =
  search_cache := MapStr.empty;;

let cache_exists k v =
  MapStr.exists (fun _k _v ->
    if k = _k then (
      v := _v;
      true
    ) else
      false
  ) !search_cache

(* find blob's content corresponding to the key *)
let find_opt ?(cache=false) t key = 
  catch (fun () ->
    Lwt_list.fold_left_s (fun (key,sha) k ->
      let obj's_tree = ref [] in
      let key_str = Key.to_string_rev key in
      if cache && cache_exists key_str obj's_tree then (
        let te = List.find (fun te -> te.name = k) !obj's_tree in
        return (k::key,te.sha)
      ) else (
        read_object t sha >>= fun obj ->
        match obj with
        | `Tree t ->
          let te = List.find (fun te -> te.name = k) t in
          search_cache := MapStr.add key_str t !search_cache;
          return (k::key,te.sha)
        | _ -> raise InvalidObject
      )
    ) ([],t.commit.tree) key >>= fun (_,sha) ->
    read_object t sha >>= function
    | `Blob c -> return (Some c)
    | _ -> return None
  ) (function |Not_found -> return None|ex -> raise ex)

let read_opt t key = 
  find_opt ~cache:true t key

let read_exn t key = 
  find_opt ~cache:true t key >>= function
  | Some v -> return v
  | None -> raise Not_found

let to_string t =
  Printf.sprintf "head: %s\n%s\n" (Sha.to_string t.head) (Commit.to_string t.commit)
