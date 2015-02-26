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
open Sexplib.Conv

let priv = ref None
let pub = ref None

let crlf = "\n"

(* keep two blocks of data in the storage: 
 * headers, content, each compressed/encrypted individually
 * Headers are stored as sexp of headers list (n*v list),
 * data_descr: 
 * offset points to the start of header/content in each block,
 * length is the length of the data in the block,
 * part_descr:
 * size is the size of the original data as it appears in the email
 * lines is the number of lines in the original email
 * Attachment are stored as individual blocks compressed/convergent encrypted
 * for deduplication
 *)

type data_descr = {offset: int; length: int} with sexp
type part_descr = {size: int; lines: int} with sexp
type email_map = {part: part_descr; header:data_descr; content: 
  [
    `Data_map of data_descr |
    `Attach_map of string(*contid*) |
    `Message_map of email_map |
    `Multipart_map of string(*boundary*) * email_map list
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
  Buffer.add_string buffer (boundary ^ suffix)

let get_hdr_attrs headers =
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
        if match_regex ~case:false v ~regx:"multipart" then (
          if match_regex ~case:false v ~regx:"boundary=\"\\([^\"]+\\)\"" then
            ("--" ^ Str.matched_group 1 v)
          else (
            let l = Str.split (Str.regexp ";") v in
            let b = List.fold_left (fun b i ->
              let i = String.trim i in
              if match_regex ~case:false i ~regx:"^boundary=\\(.+\\)$" then
                ("--" ^ Str.matched_group 1 i)
              else
                b
            ) "" l
            in
            if b <> "" then
              b
            else
              boundary
          )
        ) else
          boundary
      in
      boundary,attach,rfc822
    ) else
      boundary,attach,rfc822
  ) ("",false,false) headers

let headers_str_of_list headers =
  List.fold_left (fun acc (n,v) ->
    acc ^ n ^ ":" ^ v ^ crlf
  ) "" headers

let email_raw_content email =
  match (Email.raw_content email) with
  | Some rc -> Octet_stream.to_string rc
  | None -> ""

let email_content attachment email =
  match (Email.raw_content email) with
  | Some rc -> 
    let content = Octet_stream.to_string rc in
    let size = Bytes.length content in
    let lines = Utils.lines content in
    if srv_config.encrypt = true && attachment then (
      pub_key () >>= fun pub ->
      let (contid,content) = conv_encrypt ~compress:srv_config.compress content pub in
      return (contid,content,size,lines)
    ) else (
      let hash = get_hash content in
      return (hash,content,size,lines)
    )
  | None -> return ("","",0,0) 

let do_encrypt data =
  if srv_config.encrypt then
    pub_key () >>= fun pub ->
    return (encrypt ~compress:srv_config.compress data pub)
  else
    return data

let get_header_descr email headers_buff =
  let headers = Header.to_list (Email.header email) in
  let headers_str = headers_str_of_list headers in
  let headers_sexp_str = Sexp.to_string (sexp_of_list (fun (n,v) -> 
    sexp_of_pair sexp_of_string sexp_of_string (n,v)
  ) headers) in
  let descr = {
    offset = Buffer.length headers_buff;
    length = Bytes.length headers_sexp_str;
  } in
  let part = {
    size = Bytes.length headers_str;
    lines = Utils.lines headers_str
  } in
  Buffer.add_string headers_buff headers_sexp_str;
  (part,descr)

let do_encrypt_content email save_attachment =
  let content_buff = Buffer.create 100 in
  let headers_buff = Buffer.create 100 in
  let rec walk email multipart last_crlf totsize totlines =
    let (header_part,header_descr) = get_header_descr email headers_buff in
    let boundary,attach,rfc822 = get_hdr_attrs (Header.to_list (Email.header email)) in
    match (Email.content email) with
    | `Data _ -> 
      (* it seems that email_message doesn't parse rfc822??? I would have
       * expected rfc822 in multipart to have `Message type, so here is a hack *)
      if multipart && rfc822 then (
        let email = Email.of_string (email_raw_content email) in
        walk email multipart 2 totsize totlines >>= fun (content,size,lines) ->
        let part = {size=header_part.size+size;lines=header_part.lines+lines} in
        return (
          {
            part;
            header=header_descr; 
            content = `Message_map content;
          },totsize+part.size,totlines+part.lines)
      ) else (
        email_content attach email >>= fun (contid,content,size,lines) -> 
        if attach then ( (* consider adding Content-type: message/external-body...  *)
          save_attachment contid content >>= fun () ->
          (* +1 for crlf - header crlf content *)
          let part = {size=header_part.size+size+1;lines=header_part.lines+lines+1} in
          return (
            {
              part;
              header=header_descr;
              content=`Attach_map contid 
            },totsize+part.size,totlines+part.lines)
        ) else (
          let offset = Buffer.length content_buff in
          let length = Bytes.length content in
          Buffer.add_string content_buff content;
          let part = {size=header_part.size+size+1;lines=header_part.lines+lines+1} in
          return (
            {
              part;
              header=header_descr;
              content = `Data_map {offset;length};
            },totsize+part.size,totlines+part.lines)
        )
      )
    | `Message _ -> assert (false); (* email_parser doesn't make it??? *)
    | `Multipart elist ->
      assert (boundary <> "");
      Lwt_list.fold_left_s (fun (map,size,lines) email ->
        let size = size + (Bytes.length boundary) + 1 in
        let lines = lines + 1 in
        walk email true 2 size lines >>= fun (email_map,size,lines) ->
        return (email_map :: map,size,lines)
      ) ([],1,1) elist >>= fun (map,size,lines) -> (* 1 because first boundary starts with crlf *)
      let size = size + (Bytes.length boundary) + last_crlf in (* boundary ends
      with 2 crlf, last outermost with 1 *)
      let lines = lines + 2 in
      let part={size=header_part.size+size;lines=header_part.lines+lines} in
      return (
        {
          part;
          header=header_descr;
          content = `Multipart_map (boundary,(List.rev map))
        },totsize+part.size,totlines+part.lines)
  in
  walk email false 1 0 0 >>= fun (map,_,_) ->
  let map_sexp_str = Sexp.to_string (sexp_of_email_map map) in
  let content = Buffer.contents content_buff in
  let headers = Printf.sprintf "%04d%s%s" 
    (Bytes.length map_sexp_str) map_sexp_str (Buffer.contents headers_buff) in
  do_encrypt headers >>= fun headers ->
  do_encrypt content >>= fun content ->
  return (headers,content)

let parse (message:Mailbox.Message.t) ~save_message ~save_attachment =
  do_encrypt (Mailbox.Postmark.to_string message.postmark) >>= fun postmark ->
  do_encrypt_content message.email save_attachment >>= fun (headers,content) ->
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

let get_decrypt_attachment get_attachment contid =
  get_attachment contid >>= fun attachment ->
  priv_key() >>= fun priv ->
  return (conv_decrypt ~compressed:srv_config.compress attachment priv)

let header_of_sexp_str str =
  let sexp = Sexp.of_string str in
  let headers = list_of_sexp (fun sexp -> pair_of_sexp string_of_sexp string_of_sexp sexp) sexp in
  headers_str_of_list headers

let reassemble_email ~headers ~content ~map ~get_attachment =
  let buffer = Buffer.create 100 in
  let rec walk map = 
    let header = header_of_sexp_str (Bytes.sub headers map.header.offset map.header.length) in
    Buffer.add_string buffer header;
    Buffer.add_string buffer crlf;
    match map.content with
    | `Data_map descr -> 
      Buffer.add_string buffer (Bytes.sub content descr.offset descr.length);
      Buffer.add_string buffer crlf;
      return ()
    | `Attach_map contid ->
      get_decrypt_attachment get_attachment contid >>= fun cont ->
      Buffer.add_string buffer cont;
      Buffer.add_string buffer crlf;
      return ()
    | `Message_map emap -> walk emap
    | `Multipart_map (boundary,lmap) ->
      Buffer.add_string buffer crlf;
      Lwt_list.iter_s (fun map -> 
        add_boundary buffer ~boundary ~suffix:crlf;
        walk map
      ) lmap >>= fun () ->
      add_boundary buffer ~boundary ~suffix:("--" ^ crlf ^ crlf);
      return ()
  in
  walk map >>
  return (Buffer.contents buffer)

let do_decrypt data =
  if srv_config.encrypt then (
    priv_key() >>= fun priv ->
    return (decrypt ~compressed:srv_config.compress data priv)
  ) else
    return data

let do_decrypt_content content =
  do_decrypt content

let do_decrypt_headers headers =
  do_decrypt headers >>= fun headers ->
  let len = int_of_string (Bytes.sub headers 0 4) in
  let map_sexp_str = Bytes.sub headers 4 len in
  let map = email_map_of_sexp (Sexp.of_string map_sexp_str) in
  return (map,Bytes.sub headers (4 + len) (Bytes.length headers - 4 - len))

let restore ~get_message ~get_attachment  =
  catch (fun () ->
    get_message () >>= fun (postmark,headers,content) ->
    do_decrypt postmark >>= fun postmark ->
    do_decrypt_headers headers >>= fun (map,headers) ->
    do_decrypt_content content >>= fun content ->
    reassemble_email ~headers ~content ~map ~get_attachment >>= fun email -> 
    let email = Email.of_string email in
    return {
      Mailbox.Message.postmark=Mailbox.Postmark.of_string postmark;
      Mailbox.Message.email=email
    }
  ) (fun ex -> Printf.printf "restore exception %s\n%!" (Printexc.to_string ex); raise ex)
