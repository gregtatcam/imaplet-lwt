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
open Nocrypto
open Imap_crypto
open Server_config
open Email_message
open Regex

module MapStr = Map.Make(String)

let priv = ref None
let pub = ref None

(* each account should have it's own key,
 * should be part of the account creation TBD *)
let pub_key () =
  match !pub with
  | None -> 
    Ssl_.create_cert() >>= fun (_,priv) -> 
    let p = Rsa.pub_of_priv priv in
    pub := Some p;
    return p
  | Some pub -> return pub

let priv_key () =
  match !priv with
  | None -> 
    Ssl_.create_cert() >>= fun (_,p) -> 
    priv := Some p;
    return p
  | Some priv -> return priv

let get_hdr_attrs headers boundary =
  List.fold_left (fun (boundary,attach,base64) (n,v) ->
    let base64 =
    if match_regex ~case:false n ~regx:"Content-Transfer-Encoding" &&
        match_regex ~case:false v ~regx:"base64" then 
      true
    else
      base64
    in
    if match_regex ~case:false n ~regx:"Content-Type" then (
      let attach =
        if match_regex ~case:false v ~regx:"image\\|application\\|audio\\|video" then
          true
        else
          attach
      in
      let boundary =
        if match_regex ~case:false v ~regx:"multipart" && 
            match_regex ~case:false v ~regx:"boundary=\"?\\([^\"]+\\)\"?$" then
          Some (Str.matched_group 1 v)
        else
          boundary
      in
      boundary,attach,base64
    ) else
      boundary,attach,base64
  ) (boundary,false,false) headers

let email_content attachment base64 email =
  match (Email.raw_content email) with
  | Some rc -> 
    let content = (Octet_stream.to_string rc) in
    if srv_config.encrypt = true && attachment then (
      let mode = if Octet_stream.is_binary rc then `Binary else `Text in
      let content = 
        if base64 then 
          (Octet_stream.to_string (Octet_stream.Base64.decode ~mode rc))
        else 
          content 
      in
      pub_key () >>= fun pub ->
      return (conv_encrypt ~compress:srv_config.compress content pub)
    ) else
      return ("",Octet_stream.to_string rc)
  | None -> return ("","") 

(* there is a value in having the main headers separatelly and the message
 * separatelly as the search could be optimized when it is done on iether
 * headers or the message and not both
 * should add encrypt and compress to the message metadata TBD
 *)
let parse message =
  let do_encrypt data =
    if srv_config.encrypt then
      pub_key () >>= fun pub ->
      return (encrypt ~compress:srv_config.compress data pub)
    else
      return data
  in
  let headers_str email =
    String_monoid.to_string (Header.to_string_monoid (Email.header email)) in
  let buffer = Buffer.create 100 in
  let rec walk email boundary first attachments =
    if first = false then Buffer.add_string buffer (headers_str email);
    let boundary,attach,base64 = get_hdr_attrs (Header.to_list (Email.header email)) boundary in
    if attach = false then Buffer.add_string buffer "\n";
    match (Email.content email) with
    | `Data _ -> 
      email_content attach base64 email >>= fun (contid,content) -> 
      if attach then ( (* consider adding Content-type: message/external-body...  *)
        Buffer.add_string buffer ("X-Imaplet-External-Content: " ^ contid ^ "\n\n");
        return (MapStr.add contid content attachments )
      ) else (
        Buffer.add_string buffer content;
        return attachments
      )
    | `Message email ->
      walk email boundary false attachments
    | `Multipart elist ->
      Lwt_list.fold_left_s (fun attachments email ->
        (match boundary with
        | None -> ()
        | Some boundary -> Buffer.add_string buffer ("--" ^ boundary ^ "\n"));
        walk email boundary false attachments
      ) attachments elist
  in
  walk message.Mailbox.Message.email None true (MapStr.empty) >>= fun attachments ->
  do_encrypt (Mailbox.Postmark.to_string message.postmark) >>= fun postmark ->
  do_encrypt (headers_str message.email) >>= fun headers ->
  do_encrypt (Buffer.contents buffer) >>= fun content ->
  return (postmark,headers,content,attachments)

let email_content email =
  match (Email.raw_content email) with
  | Some rc -> Octet_stream.to_string rc
  | None -> ""

let get_hdr_attrs buffer headers boundary =
  List.fold_left (fun (boundary,extbody,base64) (n,v) ->
    if match_regex ~case:false n ~regx:"X-Imaplet-External-Content" then (
        boundary, (Some (Bytes.trim v)),base64
    ) else if match_regex ~case:false n ~regx:"Content-Type" && 
        match_regex ~case:false v ~regx:"multipart" && 
        match_regex ~case:false v ~regx:"boundary=\"?\\([^\"]+\\)\"?$" then (
      Buffer.add_string buffer (n ^ ":" ^ v ^ "\n");
      Some (Str.matched_group 1 v),extbody,base64
    ) else if match_regex ~case:false n ~regx:"Content-Transfer-Encoding" &&
        match_regex ~case:false v ~regx:"base64" then ( 
      Buffer.add_string buffer (n ^ ":" ^ v ^ "\n");
      boundary,extbody,true
    ) else (
      Buffer.add_string buffer (n ^ ":" ^ v ^ "\n");
      boundary,extbody,base64
    )
  ) (boundary,None,false) headers

let restore postmark headers content attachments =
  let do_decrypt data =
    if srv_config.encrypt then (
      priv_key() >>= fun priv ->
      return (decrypt ~compressed:srv_config.compress data priv)
    ) else
      return data
  in
  do_decrypt postmark >>= fun postmark ->
  do_decrypt headers >>= fun headers ->
  do_decrypt content >>= fun content ->
  if (MapStr.cardinal attachments) = 0 then (
    let postmark = Mailbox.Postmark.of_string postmark in
    let email = Email.of_string (headers ^ "\n" ^ content) in
    return {Mailbox.Message.postmark=postmark;Mailbox.Message.email=email}
  ) else (
    let wseq = Mailbox.With_seq.of_string (postmark ^ "\n" ^ headers ^ "\n" ^ content) in
    let buffer = Buffer.create 100 in
    Mailbox.With_seq.fold_message wseq ~f:(fun _ message ->
      let rec walk email boundary =
        let (boundary,extbody,base64) = get_hdr_attrs buffer (Header.to_list (Email.header email)) boundary in
        Buffer.add_string buffer "\n";
        match (Email.content email) with
        | `Data _ -> 
          begin
          match extbody with
          | None -> Buffer.add_string buffer ((email_content email) ^ "\n"); return()
          | Some contid ->
            let attachment = MapStr.find contid attachments in
            priv_key() >>= fun priv ->
            let content = conv_decrypt ~compressed:srv_config.compress attachment priv in
            Buffer.add_string buffer ((
            if base64 then
              String_monoid.to_string (Octet_stream.to_string_monoid
              (Octet_stream.Base64.encode (Octet_stream.of_string content)))
            else
              content
            ) ^ "\n");
            return ()
          end
        | `Message email -> 
          walk email boundary
        | `Multipart elist ->
          let add_boundary suffix = function
            | None -> ()
            | Some boundary -> Buffer.add_string buffer ("--" ^ boundary ^ suffix)
          in
          Lwt_list.iter_s (fun email ->
            add_boundary "\n" boundary;
            walk email boundary
          ) elist >>= fun () ->
          add_boundary "--\n" boundary;
          return ()
      in
      walk message.email None
    ) ~init:(return()) >>
    let postmark = Mailbox.Postmark.of_string postmark in
    let email = Email.of_string (Buffer.contents buffer) in
    return {Mailbox.Message.postmark=postmark;Mailbox.Message.email=email}
  )
