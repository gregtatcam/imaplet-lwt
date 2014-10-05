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
open Imaplet_types

let formated_capability capability =
  "CAPABILITY " ^ capability

let formated_id id =
  "ID (" ^ id ^ ")"

let to_plist l = "(" ^ l ^ ")"

let fl_to_str fl =
  match fl with
  | Flags_Answered -> "\\Answered"
  | Flags_Flagged -> "\\Flagged"
  | Flags_Deleted -> "\\Deleted"
  | Flags_Seen -> "\\Seen"
  | Flags_Recent -> "\\Recent"
  | Flags_Draft -> "\\Draft"
  | Flags_Extention e -> "\\" ^ e
  | Flags_Keyword k -> k
  | Flags_Template -> "\\Template"

let str_to_fl fl =
  if fl = "\\Answered" then
    Flags_Answered
  else if fl = "\\Flagged" then
    Flags_Flagged
  else if fl = "\\Deleted" then
    Flags_Deleted
  else if fl = "\\Seen" then
    Flags_Seen
  else if fl = "\\Recent" then
    Flags_Recent
  else if fl = "\\Draft" then
    Flags_Draft
  else if fl = "\\Template" then
    Flags_Template
  else if Regex.match_regex fl ~regx:"^\\\\Extention \\(.+\\)$" then
    Flags_Extention (Str.matched_group 1 fl)
  else 
    Flags_Keyword (fl)


let substr str ~start ~size =
  let len = String.length str in
  let str =
  if len > start then
    Str.string_after str start
  else
    str
  in
  match size with
  | None -> str
  | Some size ->
    let len = String.length str in
    if len > size then
      Str.string_before str size
    else
      str

let concat_path a1 a2 =
  if a1 <> "" && a2 <> "" then
    Filename.concat a1 a2
  else if a1 <> "" then
    a1
  else 
    a2

let message_of_string postmark email =
  let open Email_message in
  let open Email_message.Mailbox in
  {Message.postmark=Postmark.of_string postmark; 
   Message.email = Email.of_string email}

let make_email_message message =
  let open Core.Std in
  let size = String.length message in
  let headers = String.slice message 0 (if size < 1024 * 5 then size else 1024 * 5) in
  if Regex.match_regex headers ~regx:"^\\(From [^\r\n]+\\)[\r\n]+?" then ( 
    let post = Str.matched_group 1 headers in
    let email = Str.last_chars message (size - (String.length
      (Str.matched_string headers))) in
    (message_of_string post email)
  ) else (
    (* try to construct the postmark, since the message could be malformed,
    * look at a slice that should include the headers
    *)
    let from buff =
      if Regex.match_regex headers ~regx:"^From: \\([^<]+\\)<\\([^>]+\\)" then
        Str.matched_group 2 buff
      else
        "From daemon@localhost.local"
    in
    let post = ("From " ^ (from headers) ^ " " ^ (Dates.postmark_date_time ())) in
    (message_of_string post message)
  )
