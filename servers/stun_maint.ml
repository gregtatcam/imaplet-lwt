(*
 * Copyright (c) 2013-2014 Gregory Tsipenyuk <gt303@cam.ac.uk>
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

module MapStr = Map.Make(String)

type stun_record = {privateaddr:string; pubaddr:string; master:string; msgid:string; ports: int list}

(* this smtp server stun records *)
let stun_records : stun_record list ref = ref []

(* Received: smtplet #172.22.1.13#129.113.20.1#1.1.1.1#25,587,2587#; Wed, 12 Mar 2015 21:10:15 +0000\r\n *)
let stun_hdr = "Received"

let stun_regex =
  Str.regexp (stun_hdr ^ 
  ": smtplet #\\([^#]+\\)#\\([^#]+\\)#\\([^#]*\\)#\\([^#]+\\)#; \\(" ^ Regex.smtp_date_regex ^ "\\)[\r\n]+")

(* list of cached stun records for domains *)
let (cached_stun_records : stun_record list Map.Make(String).t ref) = ref MapStr.empty

(* check if 'stun' header is present *)
let find_stun_hdr headers =
  try 
    Str.search_forward stun_regex headers 0 >= 0
  with Not_found -> false

(* remove 'stun' header from the email headers *)
let remove_stun_hdr headers =
  try
    Str.replace_first stun_regex "" headers
  with _ -> headers

let hdr_len = 1_000

(* find message id or add one
 *)
let get_msg_id headers =
  if Regex.match_regex ~case:false ~regx:"message-id:[ \t]+<\\([^@]+\\)@" headers then (
    Str.matched_group 1 headers, headers
  ) else (
    let msgid = Utils.unique () in
    msgid, Printf.sprintf "Message-Id: <%s@xxxx>\r\n%s" msgid headers  
  )

(* find message id in reply-to header 
 *)
let get_msg_id_from_replyto headers =
  (* in-reply-to could have multiple message-id's TBD *)
  if Regex.match_regex ~case:false ~regx:"^in-reply-to:[ \t]+<\\([^@]+\\)@" headers then
    Str.matched_group 1 headers
  else
    ""

(* parse one 'stun' record privateaddr:pubaddr:port1,port2.. *)
let get_stun_record headers =
  let privateaddr = Str.matched_group 1 headers in
  let pubaddr = Str.matched_group 2 headers in
  let master = Str.matched_group 3 headers in
  let ports = Str.matched_group 4 headers in
  let date = Str.matched_group 5 headers in
  Log_.log `Info3 
    (Printf.sprintf "### parsed stun record priv:%s, pub:%s, master:%s, ports:%s, date:%s\n" 
    privateaddr pubaddr master ports date);
  {privateaddr; pubaddr; master; msgid = ""; ports = List.fold_left (fun acc port -> 
     (int_of_string port) :: acc
    ) [] (Str.split (Str.regexp ",") ports)
  }

(* split message into headers and content, lazy parsing,
 * just assume headers are less than 1000 bytes
 *)
let split_content content =
  let len = String.length content in
  let hdr_len = hdr_len in (* should be enough, Received is first *)
  if len > hdr_len then 
    String.sub content 0 hdr_len, String.sub content hdr_len (len - hdr_len) 
  else 
    content,""

(* parse all 'stun' header records *)
let cache_stun_records domain content =
  let headers,content = split_content content in
  let rec docache headers records =
    match domain with 
    | Some domain ->
    Log_.log `Info3 (Printf.sprintf "### caching stun records for domain %s\n" domain);
    if find_stun_hdr headers then (
      let record = get_stun_record headers in
      let headers,record =
        if record.master <> "" then (
          let (msgid,headers) = get_msg_id headers in
          (headers, {record with msgid})
        ) else (
          (headers,record)
        )
      in
      let records = record :: records in
      cached_stun_records := MapStr.add domain records !cached_stun_records;
      let headers = remove_stun_hdr headers in
      docache headers records
    ) else
      headers
    | None -> headers
  in 
  let headers = docache headers [] in
  headers ^ content

let ports_to_string ports =
  List.fold_left (fun acc port ->
    if acc = "" then 
      string_of_int port
    else
      acc ^ "," ^ (string_of_int port)
  ) "" ports

(* add 'stun' header to the email - private#public#master#ports *)
(* Received: smtplet #172.22.1.13#129.113.20.1#25,587,2587#; Wed, 12 Mar 2015 21:10:15 +0000\r\n *)
let add_header ~content ~master =
  let records = !stun_records in
  if records <> [] then (
    List.fold_left (fun content record ->
      let header = Printf.sprintf "%s: smtplet #%s#%s#%s#%s#; %s\r\n" 
        stun_hdr record.privateaddr record.pubaddr master (ports_to_string record.ports) 
        (Dates.date_time_to_email (Dates.ImapTime.now()))
      in
      (header ^ content)
    ) content records
  ) else (
    content
  )

(* find if any public address in the original email matches
 * public address of this server, if the match found then 
 * this server and the destinaion are behind the same top level
 * NAT and may be in the same private network - try to use
 * the private address to send the email.
 *)
let match_stun_records ~content ~domain =
  Log_.log `Info3 (Printf.sprintf "### matching stun records for domain %s\n" domain);
  let headers,_ = split_content content in
  let msgid = get_msg_id_from_replyto headers in
  if MapStr.mem domain !cached_stun_records then (
    let origin_stun_records = MapStr.find domain !cached_stun_records in
    let this_stun_records = !stun_records in (* this server stun records *)
    Log_.log `Info3 (Printf.sprintf "### stun record for domain %s exists, orig:%d this:%d\n" 
      domain (List.length origin_stun_records) (List.length this_stun_records));
    let found = ref None in
    (* for each domain cached stun record *)
    let _ = List.exists (fun orig_record ->
      Log_.log `Info3 (Printf.sprintf "### origin record priv:%s pub:%s\n"
        orig_record.privateaddr orig_record.pubaddr);
      (* for each this server stun record *)
      List.exists (fun this_record ->
        Log_.log `Info3 (Printf.sprintf "### this server record priv:%s pub:%s\n"
          this_record.privateaddr this_record.pubaddr);
        (* this server public address is the same as the domain public address
         * so the server and the destination domain are possibly behind the same NAT
         * also could be replying to a client's device that sent message via master
         *)
        if (orig_record.msgid = "" || 
              orig_record.msgid = msgid &&
              orig_record.master = domain) && 
              orig_record.pubaddr = this_record.pubaddr then (
          found := Some orig_record;
          Log_.log `Info3 (Printf.sprintf 
            "### found matching record for the origin private %s, public %s, ports %s\n" 
            orig_record.privateaddr orig_record.pubaddr (ports_to_string
            orig_record.ports));
          true
        ) else (
          false
        )
      ) this_stun_records
    ) origin_stun_records
    in
    !found
  ) else (
    None
  )

(* need to discover public stun servers via DNS, TBD *)
let public_stun_servers = [
  "stun.l.google.com",19302;
  "stun1.l.google.com",19302;
  "stun2.l.google.com",19302;
  "stun3.l.google.com",19302;
  "stun4.l.google.com",19302;
]

(* get this server interfaces *)
let get_interfaces () =
  Utils.get_interfaces () >>= fun intfs ->
  return (List.filter (fun i -> 
    i <> "127.0.0.1" && i <> "127.0.1.1"
  ) intfs) 

(* start stun server polling *)
let start config =
  async (fun () ->
    let rec maint time =
      Lwt_unix.sleep time >>
      get_interfaces () >>= fun intfs ->
      Lwt_list.fold_left_s (fun acc privateaddr ->
        let pubaddr = ref "" in
        (* loop over available public stun servers until mapped address is found 
         * for the private address 
         *)
        Lwt_list.exists_s (fun (stun_srv,port) ->
          Log_.log `Info3 
            (Printf.sprintf "### getting ip for stun server %s, port %d\n" stun_srv port);
          Imaplet_dns.gethostbyname stun_srv >>= fun ips ->
          if ips <> [] then (
            (* send stun request *)
            Log_.log `Info3 
              (Printf.sprintf "### sending query to stun server %s, port %d\n" (List.hd ips) port);
            Imaplet_stun.stun_request ~interface:privateaddr (List.hd ips) port >>= function
            | Some (addr,_) -> pubaddr := addr; return true
            | None -> return false
          ) else (
            return false
          )
        ) public_stun_servers >>= fun found ->
        if found then (
          Log_.log `Info3 (Printf.sprintf 
            "### found mapped server address: private %s, mapped %s\n" privateaddr !pubaddr);
          return ({pubaddr = !pubaddr; privateaddr; master = ""; msgid = ""; ports = config.smtp_port} :: acc)
        ) else (
          return acc
        )
      ) [] intfs >>= fun acc ->
      stun_records := acc;
      maint 600.
    in
    if config.stun_header then
        maint 0.
    else
      return ()
  )
