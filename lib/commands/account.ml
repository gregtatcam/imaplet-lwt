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
open Imaplet_types
open Utils

type acct_config = {
  acct_data_store : [`Irmin|`Workdir|`Mailbox|`Maildir|`Gitl]; (* type of
  storage, irmin/maildir/workdir/gitl supported *)
  acct_encrypt : bool; (* encrypt messages, default true *)
  acct_compress : bool; (* compress messages, but not attachments, default true *)
  acct_compress_attach : bool; (* compress attachments, default false *)
  acct_compress_repo : bool; (* compress repo, default false *)
  acct_auth_required: bool; (* require user authentication, priv key encrypted with password, default true *)
  acct_maildir_parse: bool; (* parse message into MIME parts when in maildir storage format, default true *)
  acct_single_store: bool; (* single-store attachments in irmin and workdir format, default true *)
  acct_hybrid: bool; (* hybrid of irmin and workdir store (store should be set to irmin, default false *)
}

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
let get_bool v n = 
  if n >= (String.length v) then
    false
  else
    match String.get v n with
    | 't' -> true
    | _ -> false

exception InvalidStoreType

let get_store = function
  | "irmin" -> `Irmin
  | "workdir" -> `Workdir
  | "maildir" -> `Maildir
  | "mailbox" -> `Mailbox
  | "gitl" -> `Gitl
  | _ -> raise InvalidStoreType

let get_config buff =
 if Str.string_match (Str.regexp
 ".*:\\(gitl\\|irmin\\|workdir\\|maildir\\|mailbox\\):\\(a[tf]\\):\\(e[tf]\\):\\(c[tf]+\\):\\(s[tf]\\):\\(h[tf]\\):\\(m[tf]\\)$") buff 0 then (
   Some {acct_data_store = get_store (Str.matched_group 1 buff);
   acct_auth_required = get_bool (Str.matched_group 2 buff) 1;
   acct_encrypt = get_bool (Str.matched_group 3 buff) 1;
   acct_compress = get_bool (Str.matched_group 4 buff) 1;
   acct_compress_attach = get_bool (Str.matched_group 4 buff) 2;
   acct_compress_repo = get_bool (Str.matched_group 4 buff) 3;
   acct_single_store = get_bool (Str.matched_group 5 buff) 1;
   acct_hybrid = get_bool (Str.matched_group 6 buff) 1;
   acct_maildir_parse = get_bool (Str.matched_group 7 buff) 1;}
 ) else
   None 

let parse_users buff user password =
  try 
   let _ = Str.search_forward (Str.regexp
   "^\\([^:]+\\):{\\([^}]+\\)}\\([^:]+\\):") buff 0 in
   let u = Str.matched_group 1 buff in
   let p = Str.matched_group 3 buff in
   let t = Str.matched_group 2 buff in
   let p =
     if t = "PLAIN" then
       p = password
     else if t = "SHA1" then
       p = (Imap_crypto.get_hash ~hash:`Sha1 password)
     else if t = "SHA256" then
       p = (Imap_crypto.get_hash ~hash:`Sha256 password)
     else
       false
   in
   if u = user && p then (
     (true,get_config buff)
   ) else
    (false,None)
  with _ ->
    (false,None)

let b64decode b64 =
   (*let buff = Str.global_replace (Str.regexp "=$") "" b64 in*)
   Cstruct.to_string (Nocrypto.Base64.decode (Cstruct.of_string b64))

let parse_user_b64 b64 =
  let buff = b64decode b64 in (** need to log this if it fails **)
  let r1 = Str.regexp "^\\([^\\]+\\)\000\\([^\\]+\\)\000\\([^\\]+\\)$" in
  let r2 = Str.regexp "^\000\\([^\\]+\\)\000\\([^\\]+\\)$" in
  if Str.string_match r1 buff 0 then (
    let u1 = Str.matched_group 1 buff in
    let u2 = Str.matched_group 2 buff in
    let p = Str.matched_group 3 buff in
    if u1 = u2 then
      Some (u1,p)
    else
      None
  ) else if Str.string_match r2 buff 0 then (
    let u = Str.matched_group 1 buff in
    let p = Str.matched_group 2 buff in
    Some (u,p)
  ) else
    None

let match_user line user =
  try
    Str.search_forward (Str.regexp_case_fold ("^" ^ user ^ ":")) line 0 = 0
  with Not_found -> false

let rec read_users r user password =
  Lwt_io.read_line_opt r >>= 
    function 
      | Some res -> 
        if match_user res user then (
          if password = None then
            return (true,get_config res)
          else (
            let (res,config) = parse_users res user (option_value_exn password) in
            return (res,config)
          )
        ) else
          read_users r user password
      | None -> return (false,None)

(** have to make users configurable **)
let authenticate_user ?(b64=false) ?(users=Install.users_path) user ?password () =
  let (user,password) =
    if b64 = false then
      (user,password)
    else (
      match password with 
      | None -> (b64decode user, None)
      | Some p -> (b64decode user, Some (b64decode p))
    )
  in
  Lwt_io.with_file ~mode:Lwt_io.Input users (fun r -> 
    read_users r user password) >>= fun (res,config) ->
  return (user,password,res,config)

let auth_user user password resp_ok resp_no =
  authenticate_user user ~password () >>= fun (_,_,res,config) ->
  if res then
    return (`Ok (Resp_Ok
    (None,Utils.formated_capability(Configuration.auth_capability)), user, password, config))
  else
    return (`Error (Resp_No (None,resp_no)))

let plain_auth text =
  match (parse_user_b64 text) with
  | Some (u,p) -> 
    authenticate_user u ?password:(Some p) () 
  | None -> return ("",None,false,None)

let _plain_auth text =
  match (parse_user_b64 text) with
  | Some (u,p) -> auth_user u p "AUTHENTICATE" "PASSWORD"
  | None -> return (`Error (Resp_No (None,"PASSWORD")))

(** TBD authenticate plain against users file **)
let authenticate auth_type text =
  match auth_type with 
    | Auth_Plain -> _plain_auth text
    | _ -> return (`Error (Resp_No (None,"Authentication Type")))

(** TBD **)
let login user password = auth_user user password "LOGIN" "PASSWORD"
