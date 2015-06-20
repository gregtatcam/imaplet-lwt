open Lwt
open Irmin_unix

(*
 * for the test to work:
 * the repo on the remote is /var/mail/accounts/test/repo
 * create git-daemon-export-ok in .git
 * run git daemon on the remote as
 * git daemon --reuseaddr --base-path=/ /
 *)

let path =
  if Array.length Sys.argv = 2 then
    Sys.argv.(1)
  else
    "git://192.168.2.2/var/mail/accounts/test/repo"

let store = Irmin.basic (module Irmin_git.FS) (module Irmin.Contents.String)

let upstream = Irmin.remote_uri path

let test () =
  let config = Irmin_git.config ~root:"/var/mail/accounts/test/repo" ~bare:false () in
  Irmin.create store config task >>= fun t ->
  Irmin.pull_exn (t "Syncing with upstream store") upstream `Update >>= fun ()
->
  Irmin.read_exn (t "get the INBOX metadata") ["imaplet";"test";"mailboxes";"INBOX";"meta"]>>= fun readme ->
  Printf.printf "%s\n%!" readme;
  return_unit

let () =
  Lwt_main.run (test ())
