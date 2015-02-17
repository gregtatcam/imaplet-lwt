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
open Sexplib
open Sexplib.Std

let priv = ref None
let pub = ref None

let crlf = "\n"

type data_descr = {offset: int; length: int} with sexp
type attach_descr = {offset: int; contid: string} with sexp
type email_map = {header:data_descr; content: 
  [
    `Data_map of data_descr |
    `Attach_map of attach_descr |
    `Message_map of email_map |
    `Multipart_map of email_map list
  ]
} with sexp

let sexp_of t =
  sexp_of_email_map t

let t_of_sexp t =
  email_map_of_sexp t

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

let add_boundary buffer ~boundary ~suffix = 
  match boundary with
  | None -> ()
  | Some boundary -> Buffer.add_string buffer ("--" ^ boundary ^ suffix)

let get_hdr_attrs headers boundary =
  List.fold_left (fun (boundary,attach,rfc822) (n,v) ->
    if match_regex ~case:false n ~regx:"Content-Type" then (
      let rfc822 =
        if match_regex ~case:false v ~regx:"message/rfc822" then
          true
        else
          false
      in
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
      boundary,attach,rfc822
    ) else
      boundary,attach,rfc822
  ) (boundary,false,false) headers

let email_raw_content email =
  match (Email.raw_content email) with
  | Some rc -> Octet_stream.to_string rc
  | None -> ""

let email_content attachment email =
  match (Email.raw_content email) with
  | Some rc -> 
    if srv_config.encrypt = true && attachment then (
      let content = Octet_stream.to_string rc 
      in
      pub_key () >>= fun pub ->
      return (conv_encrypt ~compress:srv_config.compress content pub)
    ) else
      return ("",Octet_stream.to_string rc)
  | None -> return ("","") 

let do_encrypt data =
  if srv_config.encrypt then
    pub_key () >>= fun pub ->
    return (encrypt ~compress:srv_config.compress data pub)
  else
    return data

let do_encrypt_headers email =
  let headers_sexp_str = Sexp.to_string (Header.sexp_of_t (Email.header email)) in
  do_encrypt headers_sexp_str

let do_encrypt_content email save_attachment =
  let headers_str email =
    String_monoid.to_string (Header.to_string_monoid (Email.header email)) in
  let buffer = Buffer.create 100 in
  let rec walk email boundary first multipart =
    let header_map =
      if first then
        {offset=0;length=0}
      else (
        let headers = headers_str email in
        Buffer.add_string buffer headers;
        Buffer.add_string buffer crlf;
        {offset=Buffer.length buffer;length = Bytes.length headers}
      )
    in
    let boundary,attach,rfc822 = get_hdr_attrs (Header.to_list (Email.header email)) boundary in
    match (Email.content email) with
    | `Data _ -> 
      (* it seems that email_message doesn't parse rfc822??? I would have
       * expected rfc822 in multipart to have `Message type, so here is a hack *)
      if multipart && rfc822 then (
        walk (Email.of_string (email_raw_content email)) boundary false multipart >>= fun content ->
        return {header=header_map; content = `Message_map content}
      ) else (
        email_content attach email >>= fun (contid,content) -> 
        if attach then ( (* consider adding Content-type: message/external-body...  *)
          save_attachment contid content >>
          let offset = Buffer.length buffer in
          Buffer.add_string buffer crlf;
          return {header=header_map;content=`Attach_map {offset;contid}}
        ) else (
          let offset = Buffer.length buffer in
          let length = Bytes.length content in
          Buffer.add_string buffer (content ^ crlf);
          return {header=header_map;content = `Data_map {offset;length}}
        )
      )
    | `Message _ -> assert (false); (* email_parser doesn't make it??? *)
    | `Multipart elist ->
      Buffer.add_string buffer crlf;
      Lwt_list.fold_left_s (fun map email ->
        add_boundary buffer ~boundary ~suffix:crlf ;
        walk email boundary false true >>= fun email_map ->
        return (email_map :: map)
      ) [] elist >>= fun map ->
      add_boundary buffer ~boundary ~suffix:("--" ^ crlf ^ crlf) ;
      return {header=header_map;content = `Multipart_map (List.rev map)}
  in
  walk email None true false >>= fun map ->
  let map_sexp_str = Sexp.to_string (sexp_of_email_map map) in
  let content = Printf.sprintf "%04d%s%s" (Bytes.length map_sexp_str) map_sexp_str (Buffer.contents buffer) in
  do_encrypt content 

let parse (message:Mailbox.Message.t) ~save_message ~save_attachment =
  do_encrypt (Mailbox.Postmark.to_string message.postmark) >>= fun postmark ->
  do_encrypt_headers message.email >>= fun headers ->
  do_encrypt_content message.email save_attachment >>= fun content ->
  save_message postmark headers content

(* there must be a better way to do it TBD *)
let rec printable buffer str =
  if Bytes.length str >= 76 then (
    Buffer.add_string buffer (Bytes.sub str 0 76);
    Buffer.add_string buffer crlf;
    printable buffer (Bytes.sub str 76 (Bytes.length str - 76))
  ) else (
    Buffer.add_string buffer str;
    Buffer.add_string buffer crlf;
    Buffer.contents buffer
  )

let reassemble_attachments ~content ~map ~get_attachment =
  let buffer = Buffer.create 100 in
  let rec walk start = function
    | `Data_map _ -> return start
    | `Message_map emap -> walk start emap.content
    | `Multipart_map lmap ->
      Lwt_list.fold_left_s (fun start map -> walk start map.content) start lmap
    | `Attach_map ad ->
      Buffer.add_string buffer (Bytes.sub content start (ad.offset - start));
      get_attachment ad.contid >>= fun attachment ->
      priv_key() >>= fun priv ->
      let cont = conv_decrypt ~compressed:srv_config.compress attachment priv in
      Buffer.add_string buffer cont;
      return ad.offset
  in
  walk 0 map.content >>= fun start ->
  if start <> Bytes.length content then
    Buffer.add_string buffer (Bytes.sub content start (Bytes.length content - start));
  return (Buffer.contents buffer)

let do_decrypt data =
  if srv_config.encrypt then (
    priv_key() >>= fun priv ->
    return (decrypt ~compressed:srv_config.compress data priv)
  ) else
    return data

let do_decrypt_content content =
  do_decrypt content >>= fun content ->
  let len = int_of_string (Bytes.sub content 0 4) in
  let map_sexp_str = Bytes.sub content 4 len in
  let map = email_map_of_sexp (Sexp.of_string map_sexp_str) in
  let content = Bytes.sub content (4 + len) (Bytes.length content - 4 - len) in
  return (content,map)

let do_decrypt_headers headers =
  do_decrypt headers >>= fun headers ->
  let headers = Header.t_of_sexp (Sexp.of_string headers) in
  return (String_monoid.to_string (Header.to_string_monoid headers))

let do_decrypt_headers_to_list headers =
  do_decrypt headers >>= fun headers ->
  let headers = Header.t_of_sexp (Sexp.of_string headers) in
  return (Header.to_list headers)

let restore ~get_message ~get_attachment  =
  catch (fun () ->
    get_message () >>= fun (postmark,headers,content) ->
    do_decrypt postmark >>= fun postmark ->
    do_decrypt_headers headers >>= fun headers ->
    do_decrypt_content content >>= fun (content,map) ->
    reassemble_attachments ~content ~map ~get_attachment >>= fun content -> 
    let postmark = Mailbox.Postmark.of_string postmark in
    let email = Email.of_string (headers ^ crlf ^ content) in
    return {Mailbox.Message.postmark=postmark;Mailbox.Message.email=email}
  ) (fun ex -> Printf.printf "restore exception %s\n%!" (Printexc.to_string ex); raise ex)
