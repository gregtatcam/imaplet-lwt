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
open Core.Std
open Lwt
open Imaplet_types
open Utils

(**
 CAPABILITY IMAP4rev1 LITERAL+ SASL-IR LOGIN-REFERRALS ID ENABLE IDLE SORT
 SORT=DISPLAY THREAD=REFERENCES THREAD=REFS THREAD=ORDEREDSUBJECT MULTIAPPEND
 URL-PARTIAL CATENATE UNSELECT CHILDREN NAMESPACE UIDPLUS LIST-EXTENDED
 I18NLEVEL=1 CONDSTORE QRESYNC ESEARCH ESORT SEARCHRES WITHIN CONTEXT=SEARCH
 LIST-STATUS SPECIAL-USE BINARY MOVE
**)

(** users file:
  * dovecot:{PLAIN}dovecot:/Users/dovecot:/var/mail/dovecot
**)

let parse_users buff user password =
  try 
   let _ = Str.search_forward (Str.regexp
   "^\\([^:]+\\):{\\([^}]+\\)}\\([^:]+\\):") buff 0 in
   let u = Str.matched_group 1 buff in
   let p = Str.matched_group 3 buff in
   let t = Str.matched_group 2 buff in
   if u = user && p = password && t = "PLAIN" then
    true
   else
    false
  with _ ->
    false

let parse_user_b64 b64 =
  try 
   let buff = try String.chop_suffix_exn b64 ~suffix:"=" with _ -> b64 in 
   let buff = Batteries.Base64.str_decode buff in (** need to log this if it fails **)
   let _ = Str.search_forward (Str.regexp
   "^\\([^\\]+\\)\000\\([^\\]+\\)\000\\([^\\]+\\)$") buff 0 in
   let u1 = Str.matched_group 1 buff in
   let u2 = Str.matched_group 2 buff in
   let p = Str.matched_group 3 buff in
   if u1 = u2 then
     Some (u1,p)
   else
     None
  with _ ->
    None

let rec read_users r user password =
  Lwt_io.read_line_opt r >>= 
    function 
      | Some res -> 
        if parse_users res user password then
          return (true)
        else
          read_users r user password
      | None -> return (false)

(** have to make users configurable **)
let authenticate_user ?(users=Install.users_path) user password =
  Lwt_io.with_file ~mode:Lwt_io.Input users (fun r -> read_users r user password)


let auth_user user password resp_ok resp_no =
  authenticate_user user password >>= fun res ->
  if res then
    return (Ok (Resp_Ok
    (None,Utils.formated_capability(Configuration.auth_capability)), user))
  else
    return (Error (Resp_No (None,resp_no)))

let plain_auth text =
  match (parse_user_b64 text) with
  | Some (u,p) -> auth_user u p "AUTHENTICATE" "PASSWORD"
  | None -> return (Error (Resp_No (None,"PASSWORD")))

(** TBD authenticate plain against users file **)
let authenticate auth_type text =
  printf "authenticating %s----\n%!" text;
  match auth_type with 
    | Auth_Plain -> plain_auth text
    | _ -> return (Error (Resp_No (None,"Authentication Type")))

(** TBD **)
let login user password = auth_user user password "LOGIN" "PASSWORD"
