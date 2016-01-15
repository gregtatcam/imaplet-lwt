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
open Storage_meta
open Lazy_message
open Lightparsemail

module MapStr = Map.Make(String)

(* message body without the headers *)
let _raw_content t = Content.to_string (Email.body t)

(* lazy string returns email message string, lazy email.t returns parsed email
 * message 
 *)
type maildir_email_accessor = string Lwt.t lazy_t * Email.t Lwt.t lazy_t 

module LazyMaildirEmail : LazyEmail_intf with type c = maildir_email_accessor =
  struct
    (* plain content *)
    type c = maildir_email_accessor

    type t = maildir_email_accessor

    let create c = c

    let empty = 
      (Lazy.from_fun (fun () -> return ""),Lazy.from_fun (fun () -> return (Email.empty ())))

    let email_of_t t =
      let (_,lazy_email) = t in
      Lazy.force lazy_email 

    let t_of_email e =
      (Lazy.from_fun (fun () -> return (_raw_content e)), Lazy.from_fun (fun () -> return e))

    let header ?(incl=`Map MapStr.empty) ?(excl=MapStr.empty) t =
      email_of_t t >>= fun email ->
      let headers = Headers.to_list (Email.headers email) in
      return (List.filter (fun (n,_) ->
        let dont_incl =
        match incl with
        | `Map incl -> 
          MapStr.is_empty incl = false && MapStr.mem (String.lowercase n) incl = false
        | `Regx incl ->
          Regex.match_regex ~case:false ~regx:incl n = false
        in
        (dont_incl = true ||
          MapStr.is_empty excl = false && MapStr.mem (String.lowercase n) excl = true) = false
      ) headers)

    let header_to_str ?(incl=`Map MapStr.empty) ?(excl=MapStr.empty) t =
      header ~incl ~excl t >>= fun headers ->
      return (String.concat "" (List.fold_right (fun (n,v) acc-> List.concat
        [[n;": ";v;Email_parse.crlf];acc]) headers []))

    let content t =
      email_of_t t >>= fun email ->
      return (
        match (Content.content (Email.body email)) with
        | `Data content -> `Data (Content.to_string content)
        | `Message m -> `Message (t_of_email m)
        | `Multipart lm -> `Multipart (List.map (fun m -> t_of_email m) lm)
      )
      
    let raw_content t =
      email_of_t t >>= fun email ->
      return (_raw_content email)

    (* return email message with headers *)
    let to_string ?(incl=`Map MapStr.empty) ?(excl=MapStr.empty) t =
      if incl = (`Map MapStr.empty) && excl = MapStr.empty then (
        let (lazy_str,_) = t in
        (* return raw message as is *)
        Lazy.force lazy_str
      ) else (
        (* exclude/include headers *)
        header_to_str ~incl ~excl t >>= fun headers ->
        raw_content t >>= fun content ->
        return (String.concat "" [headers;Email_parse.crlf;content])
      )

    let lines t =
      email_of_t t >>= fun (email:Email.t) ->
      return (email.mail_.body.position.lines)

    let size t =
      email_of_t t >>= fun (email:Email.t) ->
      return (email.mail_.body.position.size)

  end

let _email msg =
  Lazy.force msg >>= fun msg ->
  return (Message.email msg)

let _postmark msg = 
  Lazy.force msg >>= fun msg ->
  return (Message.postmark msg)

(* lazy string returns message string w/out postmark, lazy message.t returns parsed message
 *)
type maildir_accessor = string Lwt.t lazy_t * Message.t Lwt.t lazy_t *
mailbox_message_metadata Lwt.t lazy_t

module LazyMaildirMessage : LazyMessage_intf with type c = maildir_accessor =
  struct
    type c = maildir_accessor
    type t = maildir_accessor

    let create c = c

    let get_postmark t =
      let (_,msg,_) = t in
      _postmark msg >>= fun postmark ->
      return (Postmark.to_string postmark)

    let get_headers_block t =
      let (_,msg,_) = t in
      _email msg >>= fun email ->
      let headers = Email.headers email in
      return (Headers.to_string headers)

    let get_content_block t =
      let (_,msg,_) = t in
      _email msg >>= fun email ->
      return (_raw_content email)

    let get_email t =
      let (str,msg,_) = t in
      let lazy_email = Lazy.from_fun (fun () ->
        _email msg) in
      return (build_lazy_email_inst (module LazyMaildirEmail) (str, lazy_email))

    let get_message_metadata t =
      let (_,_,meta) = t in
      Lazy.force meta
  end
