open Lwt
open Imaplet
open Commands
open Install

let () =
  Lwt_main.run (
    let dir = Filename.concat (Sys.getcwd()) "deploy" in
    let cmd = Printf.sprintf "rm -rf ./deploy imaplet-deploy.tar.gz" in
    catch (fun() -> Lwt_unix.system cmd >>= fun _ -> return ()) (fun _ -> return()) >>
    Lwt_unix.mkdir dir 0o666 >>
    let cmd = Printf.sprintf "cd %s; cp imaplet smtplet imaplet_irmin_build imaplet_create_account imaplet_configure %s/." bin_path dir in
    Lwt_unix.system cmd >>= fun _ ->
    let cmd = Printf.sprintf "cd %s; cp server.key server.pem imaplet.cf users imaplet.pl deploy.sh imapletd %s/." data_path dir in
    Lwt_unix.system cmd >>= fun _ ->
    Lwt_unix.system "tar -czvf imaplet-deploy.tar.gz ./deploy" >>= fun _ ->
    Lwt_unix.system "chmod 666 imaplet-deploy.tar.gz" >>= fun _ ->
    Lwt_unix.system "rm -rf ./deploy" >>= fun _ ->
    return ()
  )
