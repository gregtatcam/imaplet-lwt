open Lwt
open Irmin_unix

(* for the test to work:
 * the -remote-repo is: git://192.168.2.2/var/mail/accounts/test/repo
 * the -local-repo is: /var/mail/accounts/test/repo
 * run git daemon on the remote as
 * git daemon --reuseaddr --export-all --base-path=/ /
 *)

exception InvalidCommand

let opt_val = function
  | None -> "nil"
  | Some v -> v

let opt_eq x = function
  | None -> None = x
  | Some v -> v = x

module ImapContents = 
  struct
    include Irmin.Contents.String
    let merge path ~old x y =
      let open Irmin.Merge.OP in
      Printf.printf "merging path: %s %b\n%!" (String.concat "/" path) (x = y);
      old () >>= function
      | `Conflict _ -> ok y
      | `Ok old ->
      if opt_eq x old then (
        ok y
      ) else if opt_eq y old then (
        ok x
      ) else (
        ok y 
      )
  end

module Store = Irmin_git.FS(ImapContents)(Irmin.Ref.String)(Irmin.Hash.SHA1)
module Sync = Irmin.Sync(Store)

let rec args i remote local depth =
  if i >= Array.length Sys.argv then
    remote, local, depth
  else
    match Sys.argv.(i) with 
    | "-remote-repo" -> args (i+2) (Some Sys.argv.(i+1)) local depth
    | "-local-repo" -> args (i+2) remote (Some Sys.argv.(i+1)) depth
    | "-depth" -> args (i+2) remote local (Some (int_of_string Sys.argv.(i+1)))
    | _ -> raise InvalidCommand

let commands f =
  try 
    let remote,local,depth = args 1 None None None in
    if remote = None || local = None then
      raise InvalidCommand;
    f (opt_val remote) (opt_val local) depth
  with _ ->
    Printf.printf "usage: sync -remote-repo [url] -local-repo [path] [-depth x] \n%!"

let fetch remote local depth =
  let config = Irmin_git.config ~root:local ~bare:true () in
  Store.Repo.create config >>= Store.master task >>= fun store ->
  let upstream = Irmin.remote_uri remote in
  Sync.pull_exn (store "Syncing with upstream store") ?depth upstream `Merge

let () =
  commands (fun remote local depth ->
    Lwt_main.run (
      catch (fun () -> fetch remote local depth)
        (fun ex -> Printf.printf "----- exception %s\n%!" (Printexc.to_string ex); return ())
    )
  )
