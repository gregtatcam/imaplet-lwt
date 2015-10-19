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
open Gitl_storage
open Sexplib.Conv
open Storage
open Mailbox_storage
open Maildir_storage

exception InvalidCommand
exception SystemFailed of string
exception AccountExists
exception InvalidDomain

let get_mbox_type t =
  match t with
  | "irmin" -> `Irmin
  | "workdir" -> `Workdir
  | "maildir" -> `Maildir
  | "mbox" -> `Mailbox
  | "gitl" -> `Gitl
  | _ -> raise InvalidCommand

let mbox_type_to_string t =
  match t with
  | `Irmin -> "irmin"
  | `Workdir -> "workdir"
  | `Maildir -> "maildir"
  | `Mailbox -> "mbox"
  | `Gitl -> "gitl"

let rec args i user force mbox_type ignore encrypt compress compressattch hybrid
single maildir_parse compress_repo =
  if i >= Array.length Sys.argv then
    user,force,mbox_type,ignore,encrypt,compress,compressattch,hybrid,single,maildir_parse,compress_repo
  else (
    match Sys.argv.(i) with 
    | "-u" -> args (i+2) (Some Sys.argv.(i+1)) force mbox_type ignore encrypt
    compress compressattch hybrid single maildir_parse compress_repo
    | "-f" -> args (i+1) user true mbox_type ignore encrypt compress
    compressattch hybrid single maildir_parse compress_repo
    | "-t" -> args (i+2) user force (get_mbox_type Sys.argv.(i+1)) ignore
    encrypt compress compressattch hybrid single maildir_parse compress_repo
    | "-i" -> args (i+1) user force mbox_type true encrypt compress
    compressattch hybrid single maildir_parse compress_repo
    | "-e" -> args (i+2) user force mbox_type ignore (bool_of_string
    Sys.argv.(i+1)) compress compressattch hybrid single maildir_parse compress_repo
    | "-c" -> args (i+2) user force mbox_type ignore encrypt (bool_of_string
    Sys.argv.(i+1)) compressattch hybrid single maildir_parse compress_repo
    | "-ca" -> args (i+2) user force mbox_type ignore encrypt compress
    (bool_of_string Sys.argv.(i+1)) hybrid single maildir_parse compress_repo
    | "-h" -> args (i+2) user force mbox_type ignore encrypt compress
    compressattch (bool_of_string Sys.argv.(i+1)) single maildir_parse compress_repo
    | "-s" -> args (i+2) user force mbox_type ignore encrypt compress 
    compressattch hybrid (bool_of_string Sys.argv.(i+1)) maildir_parse compress_repo
    | "-m" -> args (i+2) user force mbox_type ignore encrypt compress
    compressattch hybrid single (bool_of_string Sys.argv.(i+1)) compress_repo
    | "-cr" -> args (i+2) user force mbox_type ignore encrypt compress
    compressattch hybrid single maildir_parse (bool_of_string Sys.argv.(i+1))
    | _ -> raise InvalidCommand
  )

let usage () =
  Printf.printf "usage: imaplet_create_account -u [user:pswd] [-f] -t
  [irmin|workdir|maildir|mbox|gitl] [-i] -e [true|false] -c [true|false] -ca
  [true|false] -cr [true|false] -h [true|false] -s [true|false] -m [true|false]\n%!"

let commands f =
  try 
    let
    (user,force,mbox_type,ignore,encrypt,compress,compressattch,hybrid,single,maildir_parse,compress_repo) =
      args 1 None false `Irmin false srv_config.encrypt srv_config.compress
      srv_config.compress_attach srv_config.hybrid srv_config.single_store
      srv_config.maildir_parse srv_config.compress_repo in
    if user = None || 
      Regex.match_regex ~regx:("^\\([^:]+\\):\\([^:]+\\)$")
      (Utils.option_value_exn user) = false then
      usage ()
    else
      try 
        f (Str.matched_group 1 (Utils.option_value_exn user)) 
          (Str.matched_group 2 (Utils.option_value_exn user)) force mbox_type
          ~ignore ~encrypt ~compress ~compressattch ~hybrid ~single
          ~maildir_parse ~compress_repo
      with ex -> Printf.printf "%s\n%!" (Printexc.to_string ex)
  with _ -> usage ()

let system cmd =
  Lwt_unix.system cmd >>= function
  | Unix.WEXITED i -> return (if i = 0 then () else raise (SystemFailed cmd)) 
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

let check_domain user =
  let user,domain = Utils.parse_user user in
  match domain with
  | None -> ()
  | Some domain ->
    if List.exists (fun d -> d = domain) (Str.split (Str.regexp ";") srv_config.domain) then
      ()
    else
      raise InvalidDomain

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

let frmt_bool = function
  | true -> 't'
  | false -> 'f'

let user_config mbox_type ~encrypt ~compress ~compressattch ~hybrid ~single
~maildir_parse ~compress_repo =
  Printf.sprintf "%s:a%c:e%c:c%c%c%c:s%c:h%c:m%c"
  (mbox_type_to_string mbox_type)
  (frmt_bool srv_config.auth_required)
  (frmt_bool encrypt)
  (frmt_bool compress)
  (frmt_bool compressattch)
  (frmt_bool compress_repo)
  (frmt_bool single)
  (frmt_bool hybrid)
  (frmt_bool maildir_parse)

let set_users user pswd mbox_type ~encrypt ~compress ~compressattch ~hybrid
~single ~maildir_parse ~compress_repo =
  let new_user = 
    (Printf.sprintf "%s:{SHA256}%s::::%s:%s" user
    (Imap_crypto.get_hash ~hash:`Sha256 pswd) 
    (Utils.user_path ~path:srv_config.irmin_path ~user ())
    (user_config mbox_type ~encrypt ~compress ~compressattch ~hybrid ~single
    ~maildir_parse ~compress_repo)) in
  Utils.lines_of_file srv_config.users_path ~init:[] ~f:(fun line acc ->
    if Regex.match_regex ~case:false ~regx:("^" ^ user ^ ":") line then
      return acc
    else
      return (line :: acc)
  ) >>= fun lines ->
  let strm = Lwt_stream.of_list (List.rev (new_user::lines)) in
  Lwt_io.lines_to_file srv_config.users_path strm

let log msg =
  Lwt_io.with_file ~flags:[Unix.O_WRONLY;Unix.O_APPEND;Unix.O_CREAT] ~mode:Lwt_io.output "/tmp/log/imaplet.log"
  (fun oc -> Lwt_io.write_line oc msg)

let genrsa user pswd priv_path =
  Lwt_process.pread ~stderr:`Dev_null ~stdin:`Close ("",[|"openssl";"genrsa";"1024s"|]) >>= fun priv_key -> 
  Lwt_io.with_file ~mode:Lwt_io.output priv_path (fun w -> 
    Lwt_io.write w 
    begin
    if srv_config.auth_required then (
      let pswd = Imap_crypto.get_hash_raw (user ^ "\000" ^ pswd) in
      Imap_crypto.aes_encrypt_pswd ~pswd priv_key
    ) else (
      priv_key
    )
    end
  ) >> 
  return priv_key

let reqcert priv pem =
  Utils.exists (Filename.dirname pem) Unix.S_DIR >>= fun res ->
  let (r,w) = Lwt_unix.pipe () in
  Lwt_process.pwrite ~stderr:`Dev_null ~stdout:(`FD_move (Lwt_unix.unix_file_descr w)) ("", 
  [|"openssl"; "req"; "-x509"; "-batch"; "-new"; "-key"; "/dev/stdin"|]) priv >>
  (* the pem file doesn't appear to be flushed/closed on return, so doing it
   * this way instead of -out option to req *)
  Lwt_io.flush_all () >>= fun () ->
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input r in
  Lwt_io.read ic >>= fun str ->
  Lwt_io.close ic >>
  Lwt_io.with_file pem ~mode:Lwt_io.Output (fun oc -> 
    Lwt_io.write oc str
  )

let failed user_path msg =
  Printf.printf "%s\n%!" msg;
  match user_path with
  | Some user_path -> system ("rm -rf " ^ user_path)
  | None -> return ()

let created = ref None

let () =
  commands (fun user pswd force mbox_type ~ignore ~encrypt ~compress ~compressattch
  ~hybrid ~single ~maildir_parse ~compress_repo ->
    let user_path = Utils.user_path ~regx:"%user%.*$" ~path:srv_config.user_cert_path ~user () in
    let user_cert_path = Utils.user_path ~path:srv_config.user_cert_path ~user () in 
    let irmin_path = Utils.user_path ~path:srv_config.irmin_path ~user () in
    let mail_path = Utils.user_path ~path:srv_config.mail_path ~user () in
    let inbox_path = Utils.user_path ~path:srv_config.inbox_path ~user () in
    let priv_path = Filename.concat user_cert_path srv_config.key_name in
    let pem_path = Filename.concat user_cert_path srv_config.pem_name in
    let git_init () =
      Lwt_process.pread ~stderr:`Dev_null ~stdin:`Close 
        ("",[|"git";"init";"--bare";Filename.concat irmin_path ".git"|]) >>= fun _ -> 
      return ()
    in
    let build m mailbox keys =
      let config = {srv_config with
      inbox_path;mail_path;irmin_path;user_cert_path;encrypt;hybrid;single_store=single;maildir_parse;compress;compress_attach=compressattch;compress_repo} in
      let user = Regex.replace ~regx:"@.+$" ~tmpl:"" user in
      build_strg_inst m config user mailbox keys
    in
    let get_factory () =
      match mbox_type with
      | `Irmin -> 
        let f = build (module IrminStorage) in
        (f,irmin_path, Filename.concat irmin_path ".git", fun () -> git_init ())
      | `Workdir -> 
        let f = build (module GitWorkdirStorage) in
        (f,irmin_path, Filename.concat irmin_path "imaplet", fun () -> return ())
      | `Mailbox -> 
        let f = build (module MailboxStorage) in
        (f,mail_path,Filename.concat mail_path "mail", fun () -> return ())
      | `Maildir -> 
        let f = build (module MaildirStorage) in
        (f,mail_path,Filename.concat mail_path "Maildir", fun () -> return ())
      | `Gitl -> 
        let f = build (module GitlStorage) in
        (f,irmin_path,Filename.concat irmin_path ".git", fun () -> return ())
    in
    let check_force () =
      check_domain user;
      if force then
        system ("rm -rf " ^ user_path)
      else
        check_users user_path user pswd
    in
    Lwt_main.run (
      catch (fun () ->
        let (factory,repo_root,repo,repo_init) = get_factory () in
        check_force () >>= fun () ->
        created := Some user_path;
        catch (fun () ->
          Lwt_unix.mkdir (Filename.dirname user_path) 0o775
        ) (fun _ -> return ()) >>
        Lwt_unix.mkdir user_path 0o775 >>
        Lwt_unix.mkdir repo_root 0o775 >>
        Lwt_unix.mkdir user_cert_path 0o775 >>
        file_cmd priv_path (fun () -> 
          genrsa user pswd priv_path >>= fun key ->
	  reqcert key pem_path
        ) >>
        dir_cmd repo (fun () -> 
          repo_init () >>
          if ignore then
            return ()
          else (
            Ssl_.get_user_keys ~user ~pswd srv_config >>= fun keys ->
            factory "" keys >>= fun (module Mailbox) ->
            Mailbox.MailboxStorage.create_account Mailbox.this >>= function
            | `Exists -> raise AccountExists
            | `Ok ->
            let create_mailbox mailbox =
              factory mailbox keys >>= fun (module Mailbox) ->
              Mailbox.MailboxStorage.create_mailbox Mailbox.this >>
              Mailbox.MailboxStorage.subscribe Mailbox.this >>
              Mailbox.MailboxStorage.commit Mailbox.this
            in
            create_mailbox "INBOX" >>
            create_mailbox "Drafts" >>
            create_mailbox "Deleted Messages" >>
            create_mailbox "Sent Messages"
          )
        ) >>= fun () ->
        set_users user pswd mbox_type ~encrypt ~compress ~compressattch ~hybrid
        ~single ~maildir_parse ~compress_repo >>= fun () ->
        Printf.printf "success\n%!";
        return ()
      ) (function
        | SystemFailed msg -> failed !created ("failed: " ^ msg)
        | AccountExists -> failed !created "failed: account exists"
        | InvalidDomain -> failed !created "failed: invalid domain"
        | ex -> failed !created ("failed: " ^ (Printexc.to_string ex))
      )
    )
  )
