open Lwt
open Irmin_unix

(*
 * for the test to work:
 * the -remote-repo is: git://192.168.2.2/var/mail/accounts/test/repo
 * the -local-repo is: /var/mail/accounts/test/repo
 * create git-daemon-export-ok in .git
 * run git daemon on the remote as
 * git daemon --reuseaddr --base-path=/ /
 *)
exception InvalidCommand

let option_val = function
  | None -> assert(false)
  | Some v -> v

let rec args i remote local depth =
  if i >= Array.length Sys.argv then
    remote, local, depth
  else
    match Sys.argv.(i) with 
    | "-remote-repo" -> args (i+2) (Some Sys.argv.(i+1)) local depth
    | "-local-repo" -> args (i+2) remote (Some Sys.argv.(i+1)) depth
    | "-depth" -> args (i+2) remote local (Some (int_of_string Sys.argv.(i+1)))
    | _ -> raise InvalidCommand

let usage () =
  Printf.printf "usage: sync -remote-repo [url] -local-repo [path] [-depth x] \n%!"

let commands f =
  try 
    let remote,local,depth = args 1 None None None in
    if remote = None || local = None then
      raise InvalidCommand;
      try 
        f (option_val remote) (option_val local) depth
      with ex -> Printf.printf "%s\n" (Printexc.to_string ex)
  with _ -> usage ()

let store = Irmin.basic (module Irmin_git.FS) (module Irmin.Contents.String)

let fetch remote local depth =
  let config = Irmin_git.config ~root:local ~bare:false () in
  Irmin.create store config task >>= fun t ->
  let upstream = Irmin.remote_uri remote in
  Irmin.pull_exn (t "Syncing with upstream store") ?depth upstream `Update >>= fun ()
->
  Irmin.read_exn (t "get the INBOX metadata") ["imaplet";"test";"mailboxes";"INBOX";"meta"]>>= fun readme ->
  Printf.printf "%s\n%!" readme;
  return_unit

let () =
  commands (fun remote local depth ->
    Lwt_main.run (
      fetch remote local depth
    )
  )
