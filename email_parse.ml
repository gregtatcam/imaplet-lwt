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

let add_boundary buffer ~boundary ~suffix = 
  match boundary with
  | None -> ()
  | Some boundary -> Buffer.add_string buffer ("--" ^ boundary ^ suffix)

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
      return (conv_encrypt ~compress:false content pub)
    ) else
      return ("",Octet_stream.to_string rc)
  | None -> return ("","") 

(* there is a value in having the main headers separatelly and the message
 * separatelly as the search could be optimized when it is done on iether
 * headers or the message and not both
 * should add encrypt and compress to the message metadata TBD
 *)
let parse (message:Mailbox.Message.t) ~save_message ~save_attachment =
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
  let rec walk email boundary first map =
    if first = false then Buffer.add_string buffer (headers_str email);
    let boundary,attach,base64 = get_hdr_attrs (Header.to_list (Email.header email)) boundary in
    if attach = false then Buffer.add_string buffer "\n";
    match (Email.content email) with
    | `Data _ -> 
      email_content attach base64 email >>= fun (contid,content) -> 
      if attach then ( (* consider adding Content-type: message/external-body...  *)
        let ext = "X-Imaplet-External-Content: " ^ contid ^ "\n\n" in
        let map = (Buffer.length buffer,Bytes.length ext,base64) :: map in
        Buffer.add_string buffer ext;
        save_attachment contid content >>
        return map
      ) else (
        Buffer.add_string buffer content;
        return map
      )
    | `Message email ->
      walk email boundary false map
    | `Multipart elist ->
      Lwt_list.fold_left_s (fun map email ->
        add_boundary buffer ~boundary ~suffix:"\n" ;
        walk email boundary false map
      ) map elist >>= fun map ->
      Buffer.add_string buffer "\n";
      add_boundary buffer ~boundary ~suffix:"--\n" ;
      return map
  in
  walk message.email None true [] >>= fun map ->
  let map_sexp_str = Sexp.to_string (sexp_of_list (fun (a,b,c) -> 
      sexp_of_string 
      (String.concat " " [string_of_int a;string_of_int b; string_of_bool c])
    )  (List.rev map)) in
  let content = Printf.sprintf "%04d%s%s" (Bytes.length map_sexp_str) map_sexp_str (Buffer.contents buffer) in
  do_encrypt (Mailbox.Postmark.to_string message.postmark) >>= fun postmark ->
  do_encrypt (headers_str message.email) >>= fun headers ->
  do_encrypt content >>= fun content ->
  save_message postmark headers content

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

let restore ~get_message ~get_attachment  =
  catch (fun () ->
  let do_decrypt data =
    if srv_config.encrypt then (
      priv_key() >>= fun priv ->
      return (decrypt ~compressed:srv_config.compress data priv)
    ) else
      return data
  in
  get_message () >>= fun (postmark,headers,content) ->
  do_decrypt postmark >>= fun postmark ->
  do_decrypt headers >>= fun headers ->
  do_decrypt content >>= fun content ->
  let len = int_of_string (Bytes.sub content 0 4) in
  let map_sexp_str = Bytes.sub content 4 len in
  let map = list_of_sexp (fun sexp -> 
    let str = replace ~regx:"\"" ~tmpl:"" (Sexp.to_string sexp) in
    let parts = Str.split (Str.regexp " ") str in
      (int_of_string (List.nth parts 0),
       int_of_string (List.nth parts 1),
       bool_of_string (List.nth parts 2))
  ) (Sexp.of_string map_sexp_str) in
  let content = Bytes.sub content (4 + len) (Bytes.length content - 4 - len) in
  begin
  if List.length map = 0 then ( (* no attachments *)
    return content
  ) else (
    let buffer = Buffer.create 100 in
    Lwt_list.fold_left_s (fun start (offset,size,base64) ->
      Buffer.add_string buffer ((Bytes.sub content start (offset - start)) ^ "\n\n");
      let header = Bytes.sub content offset size in
      let _ = match_regex ~case:false header ~regx:"^X-Imaplet-External-Content: \\([^\n]+\\)\n\n$" in
      let contid = Str.matched_group 1 header in
      get_attachment contid >>= fun attachment ->
      priv_key() >>= fun priv ->
      let cont = conv_decrypt ~compressed:false attachment priv in
      Buffer.add_string buffer ((
      if base64 then (
        (*
        String_monoid.to_string (Octet_stream.to_string_monoid
        (Octet_stream.Base64.encode (Octet_stream.of_string cont)))
        *)
        (* there must be a better way to do it TBD *)
        let rec printable buffer str =
          if Bytes.length str >= 76 then (
            Buffer.add_string buffer (Bytes.sub str 0 76);
            Buffer.add_string buffer "\n";
            printable buffer (Bytes.sub str 76 (Bytes.length str - 76))
          ) else (
            Buffer.add_string buffer str;
            Buffer.add_string buffer "\n";
            Buffer.contents buffer
          )
        in
        printable (Buffer.create 100) (Cstruct.to_string (Nocrypto.Base64.encode (Cstruct.of_string cont)))
      ) else
        cont
      ) ^ "\n");
      return (offset + size)
    ) 0 map >>= fun start ->
    if start <> Bytes.length content then
      Buffer.add_string buffer (Bytes.sub content start (Bytes.length content - start));
    return (Buffer.contents buffer)
  )
  end >>= fun content -> 
  let postmark = Mailbox.Postmark.of_string postmark in
  let email = Email.of_string (headers ^ "\n" ^ content ^ "\n") in
  return {Mailbox.Message.postmark=postmark;Mailbox.Message.email=email}
  ) (fun ex -> Printf.printf "restore exception %s\n%!" (Printexc.to_string ex);
  raise ex)
