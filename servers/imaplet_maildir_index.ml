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
open Sexplib
open Maildir_storage
open Storage_meta
open Imaplet_types
open Parsemail
open Mail_file_name

exception InvalidCommand

let get_mailbox_structure str =
  if Regex.match_regex str ~regx:"^maildir:\\([^:]+\\)\\(:fs\\)?$" then (
    let dir = (Str.matched_group 1 str) in
    let fs = try let _ = Str.matched_group 2 str in true with _ -> false in
    `Maildir (dir,fs)
  ) else
    raise InvalidCommand

(* -u : user
 * -m : mailbox structure
 *      mbox:inbox-path:mailboxes-path
 *      maildir:maildir-path
 *)
let rec args i user mailbox =
  if i >= Array.length Sys.argv then
    user,mailbox
  else
    match Sys.argv.(i) with 
    | "-m" -> args (i+2) user (Some (get_mailbox_structure Sys.argv.(i+1)))
    | "-u" -> args (i+2) (Some Sys.argv.(i+1)) mailbox
    | _ -> raise InvalidCommand

(* support existing maildir only *)
let usage () =
  Printf.printf "usage: imaplet_maildir_index -u [user] -m [maildir:mailboxes-path]\n%!"

let commands f =
  try 
    let user,mbx = args 1 None None in
    if user = None || mbx = None then
      usage ()
    else
      try 
        f (Utils.option_value_exn user) (Utils.option_value_exn mbx)
      with ex -> Printf.printf "%s\n%!" (Printexc.to_string ex)
  with _ -> usage ()

let find_flag flag flags =
  try
    let _ = List.find (fun f -> f = flag) flags in true
  with Not_found -> false

let get_message file =
  Utils.with_file file ~flags:[Unix.O_RDONLY] ~perms:0o660 ~mode:Lwt_io.Input 
  ~f:(fun ci ->
    Lwt_io.read ci >>= fun message ->
      if Regex.match_regex ~regx:",M=[0-9]+:2,[a-zA-Z]+$" file then
      return (Mailbox.Message.t_of_sexp (Sexp.of_string message))
    else
      return (Utils.make_email_message message)
  ) 

let write_message file message =
  Utils.with_file file ~flags:[Unix.O_WRONLY;Unix.O_TRUNC] ~perms:0o660 ~mode:Lwt_io.Output 
  ~f:(fun co ->
    let sexp = Mailbox.Message.sexp_of_t message in
    Lwt_io.write co (Sexp.to_string sexp)
  ) 

let build_index user maildir_root mailbox =
  let mailbox_path = Filename.concat maildir_root mailbox in
  let path = Filename.concat mailbox_path "cur" in
  let strm = Lwt_unix.files_of_directory path in
  create_file ~overwrite:true (Filename.concat mailbox_path "imaplet.uidlst") >>
  create_file ~overwrite:true (Filename.concat mailbox_path "imaplet.keywords") >>
  create_file ~overwrite:true (Filename.concat mailbox_path "imaplet.meta") >>
  create_file ~overwrite:true (Filename.concat mailbox_path "imaplet.subscribe") >>= fun () ->
  Lwt_stream.fold_s (fun i acc -> 
    if i = "." || i = ".." then
      return acc
    else (
      return (i :: acc)
    )
  ) strm [] >>= fun messages ->
  let messages = List.sort String.compare messages in (* hopefully in chron order *)
  Lwt_list.fold_left_s (fun metadata file ->
    Printf.printf "processing %s\n%!" file;
    (*1412955687.M633681P37514.dhcp-172-17-157-219.eduroam.wireless.private.cam.ac.uk,S=1043,W=1062:2,STa*)
    let _ = Regex.match_regex
    ~regx:"^\\([^.]+\\)\\([^,]+\\),S=\\([0-9]+\\),\\([^:]+\\):2,\\([a-zA-z]+\\)$" file in
    let date = Pervasives.float_of_string ((Str.matched_group 1 file) ^ ".") in
    let size = int_of_string (Str.matched_group 3 file) in
    let flags = flags_of_map_str "" (Str.matched_group 5 file) in
    let new_name = make_message_file_name "" 
      {uid=metadata.uidnext;modseq=Int64.zero;internal_date=Dates.ImapTime.of_float date;size;flags;}
    in
    append_uidlist (Filename.concat mailbox_path "imaplet.uidlst") metadata.uidnext new_name >>
    get_message (Filename.concat path file) >>= fun message ->
    Lwt_unix.rename (Filename.concat path file) (Filename.concat path new_name) >>= fun () ->
    write_message (Filename.concat path new_name) message >>= fun () ->
    let funseen = find_flag Flags_Seen flags = false in
    return {metadata with
      uidnext = metadata.uidnext + 1;
      count = metadata.count + 1;
      modseq = Int64.add metadata.modseq Int64.one;
      unseen = if metadata.unseen = 0 && funseen then metadata.count + 1 else metadata.unseen;
      nunseen = if funseen then metadata.nunseen + 1 else metadata.nunseen;
      recent = if find_flag Flags_Recent flags then metadata.recent + 1 else metadata.recent;
    }
  ) (empty_mailbox_metadata ~uidvalidity:(new_uidvalidity()) ()) messages >>= fun metadata ->
    write_mailbox_metadata (Filename.concat mailbox_path "imaplet.meta") metadata

let build_all_index user maildir_root =
  build_index user maildir_root "" >>= fun () ->
  let strm = Lwt_unix.files_of_directory maildir_root in
  Lwt_stream.iter_s (fun i ->
    if i = "." || i = ".." || Regex.match_regex i ~regx:"^\\..+[^.]$" = false then
      return ()
    else (
      build_index user maildir_root i
    )
  ) strm

let () =
  commands (fun user mbx ->
    Lwt_main.run (
      catch ( fun () ->
        match mbx with
        | `Maildir (mailboxes,fs) -> build_all_index user mailboxes
      )
      (fun ex -> Printf.printf "exception: %s %s\n%!" (Printexc.to_string ex)
      (Printexc.get_backtrace());return())
    )
  )
