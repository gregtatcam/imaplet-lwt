(*
 * Copyright (c) 2013-2015 Gregory Tsipenyuk <gregtsip@cam.ac.uk>
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
open Imaplet
open Commands
open Server_config
open Irmin_core
open Irmin_storage

exception InvalidCommand
exception SystemFailed of string
exception AccountExists

let rec args i user force =
  if i >= Array.length Sys.argv then
    user,force
  else
    match Sys.argv.(i) with 
    | "-u" -> args (i+2) (Some Sys.argv.(i+1)) force
    | "-f" -> args (i+1) user true
    | _ -> raise InvalidCommand

let usage () =
  Printf.printf "usage: imaplet_create_account -u [user:pswd] [-f]\n%!"

let commands f =
  try 
    let (user,force) = args 1 None false in
    if user = None || 
      Regex.match_regex ~regx:("^\\([^:]+\\):\\([^:]+\\)$")
      (Utils.option_value_exn user) = false then
      usage ()
    else
      try 
        f (Str.matched_group 1 (Utils.option_value_exn user)) 
          (Str.matched_group 2 (Utils.option_value_exn user)) force
      with ex -> Printf.printf "%s\n%!" (Printexc.to_string ex)
  with _ -> usage ()

let system cmd =
  Lwt_unix.system cmd >>= function
  | WEXITED i -> return (if i = 0 then () else raise (SystemFailed cmd)) 
  | _ -> raise (SystemFailed cmd)

let dir f =
  Utils.exists f Unix.S_DIR

let file f =
  Utils.exists f Unix.S_REG

let file_cmd f cmd =
  file f >>= fun res ->
  if res then 
    return ()
  else
    cmd ()

let dir_cmd f cmd =
  dir f >>= fun res ->
  if res then 
    return ()
  else
    cmd ()

(*
imaplet:{PLAIN}imaplet:501:::/Users/imaplet
*)
let check_users user_path user pswd =
  dir user_path >>= fun res ->
  if res then raise AccountExists;
  Utils.lines_of_file srv_config.users_path ~init:() ~f:(fun line _ ->
    if Regex.match_regex ~case:false ~regx:("^" ^ user ^ ":") line then
      raise AccountExists;
    return ()
  )

let set_users user pswd =
  let new_user = 
    (Printf.sprintf "%s:{SHA256}%s::::%s" user
    (Imap_crypto.get_hash ~hash:`Sha256 pswd) 
    (Regex.replace ~regx:"%user%" ~tmpl:user srv_config.irmin_path)) in
  Utils.lines_of_file srv_config.users_path ~init:[] ~f:(fun line acc ->
    if Regex.match_regex ~case:false ~regx:("^" ^ user ^ ":") line then
      return acc
    else
      return (line :: acc)
  ) >>= fun lines ->
  let strm = Lwt_stream.of_list (List.rev (new_user::lines)) in
  Lwt_io.lines_to_file srv_config.users_path strm

let log msg =
  Lwt_io.with_file ~flags:[O_WRONLY;O_APPEND;O_CREAT] ~mode:Lwt_io.output "/tmp/log/imaplet.log"
  (fun oc -> Lwt_io.write_line oc msg)

let genrsa user pswd priv =
  Lwt_process.pread ~stderr:`Dev_null ("openssl",[|"genrsa";"1024s"|]) >>= fun key -> 
  Lwt_io.with_file ~mode:Lwt_io.output priv (fun w -> 
    Lwt_io.write w 
    begin
    if srv_config.auth_required then (
      let pswd = Imap_crypto.get_hash_raw (user ^ "\000" ^ pswd) in
      Imap_crypto.aes_encrypt_pswd ~pswd key
    ) else (
      key
    )
    end
  ) >>
  return key

let reqcert priv pem =
  Lwt_process.pwrite ("openssl", 
    [|"req"; "-x509"; "-batch"; "-new"; "-key"; "/dev/stdin"; "-out"; pem|]) priv 

let failed user_path msg =
  Printf.printf "%s\n%!" msg;
  match user_path with
  | Some user_path -> system ("rm -rf " ^ user_path)
  | None -> return ()

let created = ref None

let () =
  commands (fun user pswd force ->
    let user_path = Regex.replace ~regx:"%user%.*$" ~tmpl:user srv_config.user_cert_path in
    let cert_path = Regex.replace ~regx:"%user%" ~tmpl:user srv_config.user_cert_path in
    let irmin_path = Regex.replace ~regx:"%user%" ~tmpl:user srv_config.irmin_path in
    let priv_path = Filename.concat cert_path srv_config.key_name in
    let pem_path = Filename.concat cert_path srv_config.pem_name in
    Lwt_main.run (
      catch (fun () ->
        (if force then
          system ("rm -rf " ^ user_path)
        else
          check_users user_path user pswd) >>= fun () ->
        created := Some user_path;
        Lwt_unix.mkdir user_path 0o775 >>
        Lwt_unix.mkdir irmin_path 0o775 >>
        Lwt_unix.mkdir cert_path 0o775 >>
        file_cmd priv_path 
          (fun () -> genrsa user pswd priv_path >>= fun key ->
	             reqcert key pem_path) >>
          dir_cmd (Filename.concat irmin_path ".git") 
          (fun () -> 
            system ("git init " ^ irmin_path) >>= fun () ->
            let ac = UserAccount.create srv_config user in
            UserAccount.create_account ac >>= fun _ ->
            Ssl_.get_user_keys ~user ~pswd srv_config >>= fun keys ->
            let create_mailbox mailbox =
              IrminStorage.create srv_config user mailbox keys >>= fun ist ->
              IrminStorage.create_mailbox ist >>
              IrminStorage.subscribe ist >>
              IrminStorage.commit ist
            in
            create_mailbox "INBOX" >>
            create_mailbox "Drafts" >>
            create_mailbox "Deleted Messages" >>
            create_mailbox "Sent Messages"
          ) >>
          set_users user pswd >>= fun () ->
          Printf.printf "success\n%!";
          return ()
      ) (function
        | SystemFailed msg -> failed !created ("system failed " ^ msg)
        | AccountExists -> failed !created "account exists failed"
        | ex -> failed !created ("failed " ^ (Printexc.to_string ex)))
    )
  )
