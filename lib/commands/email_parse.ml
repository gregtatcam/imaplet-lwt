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
open Regex
open Parsemail
open Sexplib
open Sexplib.Conv

exception EmptyPrivateKey

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

let headers_str_of_list headers transform =
  transform (`Headers (List.fold_left (fun acc (n,v) ->
    acc ^ n ^ ":" ^ v ^ crlf
  ) "" headers))

let email_raw_content email =
  match (Email.raw_content email) with
  | Some rc -> Octet_stream.to_string rc
  | None -> ""

let email_content pub_key config attachment email transform =
  match (Email.raw_content email) with
  | Some rc -> 
    let content = Octet_stream.to_string rc in
    let content = 
    if attachment then 
      transform (`Attachment content) 
    else 
      transform (`Body content) 
    in
    let size = Bytes.length content in
    let lines = Utils.lines content in
    if config.encrypt && attachment then (
      let (_(*contid*),content) = conv_encrypt ~compress:config.compress_attach content pub_key in
      return (content,size,lines)
    ) else if config.compress_attach && attachment then ( (*content compressed separately *)
      return (do_compress content, size, lines)
    ) else (
      return (content,size,lines)
    )
  | None -> return ("",0,0) 

let do_encrypt pub_key config data =
  if config.encrypt then (
    return (encrypt ~compress:config.compress data pub_key)
  ) else if config.compress then (
    return (do_compress data)
  ) else (
    return data
  )

let get_header_descr headers headers_buff transform =
  let headers_str = headers_str_of_list headers transform in
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

(* parse the email message into MIME parts
 * all headers are concat together and saved as a separate blob.
 * attachments are saved each into a separate blob.
 * all other content is concat together, along with a map that references
 * all parts, their storage location, and their location inside the message,
 * and all saved into a separate blob. all blobs are saved under
 * user/Storage/hash key where the hash is the hash of the whole message
 *)
let do_encrypt_content pub_key config email save_attachment hash transform =
  let content_buff = Buffer.create 100 in
  let headers_buff = Buffer.create 100 in
  let rec walk email multipart last_crlf totsize totlines attachments =
    let headers = Header.to_list (Email.header email) in
    let (header_part,header_descr) = get_header_descr headers headers_buff transform in
    let boundary,attach,rfc822 = get_hdr_attrs headers in
    match (Email.content email) with
    | `Data _ -> 
      (* it seems that email_message doesn't parse rfc822??? I would have
       * expected rfc822 in multipart to have `Message type, so here is a hack *)
      if multipart && rfc822 then (
        let email = Email.of_string (email_raw_content email) in
        walk email multipart 2 0 0 attachments >>= fun (content,size,lines,attachments) ->
        let part = 
          if multipart then 
            {size=size;lines=lines}
          else
            {size=header_part.size+size+1;lines=header_part.lines+lines+1}
        in
        return (
          {
            part;
            header=header_descr; 
            content = `Message_map content;
          },totsize+part.size,totlines+part.lines,attachments)
      ) else (
        (* don't need to key attachments or other parts by the content hash
         * all parts have the root key - message hash, then the subkeys are
         * the count, 0 - postmark, 1 - headers, 2 - content, 3+ - attachments
         * just need to keep the number of attachments
         *)
        email_content pub_key config attach email transform >>= fun (content,size,lines) -> 
        if attach then ( (* consider adding Content-type: message/external-body...  *)
          let attach_contid = (string_of_int (3 + attachments)) in
          save_attachment hash attach_contid content >>= fun () ->
          (* +1 for crlf - header crlf content *)
          let part = 
          if multipart then 
            {size=size;lines=lines}
          else
            {size=header_part.size+size+1;lines=header_part.lines+lines+1}
          in
          return (
            {
              part;
              header=header_descr;
              content=`Attach_map attach_contid 
            },totsize+part.size,totlines+part.lines,1 + attachments)
        ) else (
          let offset = Buffer.length content_buff in
          let length = Bytes.length content in
          Buffer.add_string content_buff content;
          let part = 
          if multipart then 
            {size=size;lines=lines}
          else
            {size=header_part.size+size+1;lines=header_part.lines+lines+1}
          in
          return (
            {
              part;
              header=header_descr;
              content = `Data_map {offset;length};
            },totsize+part.size,totlines+part.lines,attachments)
        )
      )
    | `Message _ -> assert (false); (* email_parser doesn't make it??? *)
    | `Multipart elist ->
      assert (boundary <> "");
      Lwt_list.fold_left_s (fun (map,size,lines,attachments) email ->
        let size_ = size + (Bytes.length boundary) + 1 in
        let lines_ = lines + 1 in
        walk email true 2 0 0 attachments >>= fun (email_map,size,lines,attachments) ->
        return (email_map :: map,size_+size,lines_+lines,attachments)
      ) ([],1,1,attachments) elist >>= fun (map,size,lines,attachments) -> (* 1 because first boundary starts with crlf *)
      let size = size + (Bytes.length boundary) + last_crlf in (* boundary ends
      with 2 crlf, last outermost with 1 *)
      let lines = lines + 2 in
      let part=
        if multipart then 
          {size=size;lines=lines}
        else
          {size=header_part.size+size+1;lines=header_part.lines+lines+1} in
      return (
        {
          part;
          header=header_descr;
          content = `Multipart_map (boundary,(List.rev map))
        },totsize+part.size,totlines+part.lines,attachments)
  in
  walk email false 1 0 0 0 >>= fun (map,_,_,attachments) ->
  let map_sexp_str = Sexp.to_string (sexp_of_email_map map) in
  let content = Buffer.contents content_buff in
  let headers = Printf.sprintf "%07d%s%s" 
    (Bytes.length map_sexp_str) map_sexp_str (Buffer.contents headers_buff) in
  do_encrypt pub_key config headers >>= fun headers ->
  do_encrypt pub_key config content >>= fun content ->
  return (headers,content,attachments)

let default_transform = function
  | `Postmark p -> p
  | `Headers h -> h
  | `Body b -> b
  | `Attachment a -> a

let parse ?(transform=default_transform) pub_key config message ~save_message ~save_attachment =
  let hash = Imap_crypto.get_hash message in
  let message = Utils.make_email_message message in
  do_encrypt pub_key config 
    (transform (`Postmark (Mailbox.Postmark.to_string message.Mailbox.Message.postmark))) >>= fun postmark ->
  do_encrypt_content pub_key config message.Mailbox.Message.email save_attachment hash transform >>= fun (headers,content, attachments) ->
  save_message hash postmark headers content attachments >>
  return hash

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

let get_decrypt_attachment priv_key config get_attachment contid =
  get_attachment contid >>= fun attachment ->
  if config.encrypt then (
    return (conv_decrypt ~compressed:config.compress_attach attachment priv_key)
  ) else if config.compress_attach then (
    return (do_uncompress attachment)
  ) else (
    return attachment
  )

let header_of_sexp_str str =
  let sexp = Sexp.of_string str in
  let headers = list_of_sexp (fun sexp -> pair_of_sexp string_of_sexp string_of_sexp sexp) sexp in
  headers_str_of_list headers (function | `Headers h -> h)

let reassemble_email priv_key config ~headers ~content ~map ~get_attachment =
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
      get_decrypt_attachment priv_key config get_attachment contid >>= fun cont ->
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

let do_decrypt priv_key config data =
  if config.encrypt then (
    return (decrypt ~compressed:config.compress data priv_key)
  ) else if config.compress then (
    return (do_uncompress data)
  ) else
    return data

let do_decrypt_content priv_key config content =
  do_decrypt priv_key config content

let do_decrypt_headers priv_key config headers =
  do_decrypt priv_key config headers >>= fun headers ->
  let len = int_of_string (Bytes.sub headers 0 7) in
  let map_sexp_str = Bytes.sub headers 7 len in
  let map = email_map_of_sexp (Sexp.of_string map_sexp_str) in
  return (map,Bytes.sub headers (7 + len) (Bytes.length headers - 7 - len))

let restore priv_key config ~get_message ~get_attachment  =
  catch (fun () ->
    get_message () >>= fun (postmark,headers,content) ->
    do_decrypt priv_key config postmark >>= fun postmark ->
    do_decrypt_headers priv_key config headers >>= fun (map,headers) ->
    do_decrypt_content priv_key config content >>= fun content ->
    reassemble_email priv_key config ~headers ~content ~map ~get_attachment >>= fun email -> 
    let email = Email.of_string email in
    return {
      Mailbox.Message.postmark=Mailbox.Postmark.of_string postmark;
      Mailbox.Message.email=email
    }
  ) (fun ex -> Printf.printf "restore exception %s\n%!" (Printexc.to_string ex); raise ex)

let message_to_blob config keys message =
  let add buffer content (offset,acc) =
    Buffer.add_string buffer content;
    let len = String.length content in
    (offset+len,(offset,len) :: acc)
  in
  let (pub_key,_) = keys in
  let strm_attach,push_attach = Lwt_stream.create () in
  let buffer = Buffer.create 100 in
  let buffer_out = Buffer.create 100 in
  parse pub_key config message ~save_message:(fun msg_hash postmark headers content attachments ->
    push_attach None;
    let acc = add buffer postmark (0,[]) in
    let acc = add buffer headers acc in
    let acc = add buffer content acc in
    Lwt_stream.fold (fun attach acc ->
      add buffer attach acc
    ) strm_attach acc >>= fun (_,acc) ->
    let acc = List.rev acc in
    let open Sexplib.Conv in
    let header = Sexplib.Sexp.to_string (sexp_of_list (fun a -> sexp_of_pair sexp_of_int sexp_of_int a) acc) in
    Buffer.add_string buffer_out (Printf.sprintf "%06d" (String.length header));
    Buffer.add_string buffer_out header;
    Buffer.add_buffer buffer_out buffer;
    return ()
  ) 
  ~save_attachment:(fun msg_hash contid attachment ->
    push_attach (Some attachment);
    return ()
  ) >>= fun hash ->
  return (hash,(Buffer.contents buffer_out))

let message_unparsed_to_blob config keys message =
  let hash = Imap_crypto.get_hash message in
  let (pub_key,_) = keys in
  do_encrypt pub_key config message >>= fun content ->
  return (hash,content)

let message_unparsed_from_blob config keys message =
  let (_,priv) = keys in
  let priv = Utils.option_value_exn ~ex:EmptyPrivateKey priv in
  do_decrypt priv config message 

(* lazy_read_message lazily reads the message from the storage
 * lazy_read_metadata - same for metadata
 *)
let message_from_blob config keys lazy_read_message f =
  let open Sexplib in
  let open Sexplib.Conv in
  let (_,priv) = keys in
  let priv = Utils.option_value_exn ~ex:EmptyPrivateKey priv in
  let lazy_header = Lazy.from_fun (fun () -> 
    (* if reading headers then makes sense to read the whole message, no
     * benefit to do random access *)
    Lazy.force lazy_read_message >>= fun message ->
    let len = int_of_string (String.sub message 0 6) in
    let chunk = String.sub message 6 len in
    let header = list_of_sexp (fun s -> pair_of_sexp int_of_sexp int_of_sexp s)
      (Sexp.of_string chunk) in
    return (6+len,header,message)
  ) in
  let get_chunk n =
    Lazy.force lazy_header >>= fun (base,header,message) ->
    let (offset,len) = List.nth header n in
    return (String.sub message (base + offset) len)
  in
  let get_chunk_str str =
    get_chunk (int_of_string str)
  in
  let postmark () =
    get_chunk 0 >>= fun _postmark ->
    do_decrypt priv config _postmark 
  in
  let headers () =
    get_chunk 1 >>= fun _headers ->
    do_decrypt_headers priv config _headers 
  in
  let content () =
    get_chunk 2 >>= fun _content ->
    do_decrypt_content priv config _content 
  in
  let attachment =
    get_decrypt_attachment priv config get_chunk_str
  in
  f postmark headers content attachment
