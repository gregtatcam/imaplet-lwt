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

let revision = "IMAP4rev1"
let literal = "LITERAL+"
let sasl = "SASL-IR"
let login'ref = "LOGIN-REFERRALS"
let id = "ID"
let enable = "ENABLE"
let idle = "IDLE"
let starttls = "STARTTLS"
let auth = "AUTH=PLAIN"
let login'disabled = "LOGINDISABLED"
let sort = "SORT"
let sort'display = "SORT=DISPLAY"
let thread'references = "THREAD=REFERENCES"
let thread'orderedsubject = "THREAD=ORDEREDSUBJECT"
let multiappend = "MULTIAPPEND"
let url'partial= "URL-PARTIAL"
let catenate = "CATENATE"
let unselect = "UNSELECT"
let children = "CHILDREN"
let namespace = "NAMESPACE"
let uidplus = "UIDPLUS"
let list'extended = "LIST-EXTENDED"
let i18nlevel = "I18NLEVEL=1"
let condstore = "CONDSTORE"
let qresync = "QRESYNC"
let esearch = "ESEARCH"
let esort = "ESORT"
let searches = "SEARCHES"
let within = "WITHIN"
let compress = "COMPRESS=DEFLATE"
let context'search = "CONTEXT=SEARCH"
let list'status = "LIST-STATUS"
let special'use = "SPECIAL-USE"
let binary = "BINARY"
let move = "MOVE"

let notauth_cap = [revision;literal; sasl; login'ref; id; enable; idle;starttls;
auth]
let auth_cap = [revision;literal; sasl; login'ref; id; enable; idle;starttls;auth;
  login'disabled; sort; sort'display; thread'references; thread'orderedsubject;
  multiappend; url'partial; catenate; unselect; children; namespace; uidplus;
  list'extended; i18nlevel; condstore; qresync; esearch; esort; searches;
  within; context'search; list'status; binary; move
]
let auth_cap = List.concat [notauth_cap;[condstore]]

let capability = String.concat " " notauth_cap

let auth_capability =  String.concat " " auth_cap

let id = "\"name\" \"Imaplet\""

let max_message_in_memory_size = 0 (**10_240**)

let mailboxes mail_path name = 
  try
  let l = Str.split (Str.regexp "@") mail_path in
   (List.nth l 0) ^ name ^ (List.nth l 1)
  with _ -> mail_path

let get_mbox_flags =
  (["\\Answered"; "\\Flagged"; "\\Deleted"; "\\Seen"; "\\Draft"; "$NotJunk";
  "NotJunk"],
  ["\\Answered"; "\\Flagged"; "\\Deleted"; "\\Seen"; "\\Draft"; "$NotJunk";
  "NotJunk"; "\\*"])

(* smtp *)
let smtp_srv_exec = Install.smtp_srv_exec

let smtp_backlog = 10
