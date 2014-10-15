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
let match_regex_i ?(case=true) str ~regx = 
  try
    let regexp =
    (
      if case = false then
        Str.regexp_case_fold regx
      else
        Str.regexp regx
    ) in
    (Str.search_forward regexp str 0)
  with _ ->
    (-1)

let match_regex ?(case=true) str ~regx = 
  let i = match_regex_i ~case str ~regx in
  (i >= 0)

let replace ~regx ~tmpl str =
  Str.global_replace (Str.regexp regx) tmpl str
  
let dq = "\""

let group re = "\\(" ^ re ^ "\\)"

let orx re1 re2 = re1 ^ "\\|" ^ re2

let quote re = 
  if match_regex re ~regx:"^\"[^\"]*\"$" then
    re
  else
    dq ^ re ^ dq

let dequote re =
  replace ~regx:"\"" ~tmpl:"" re

let list_of re = "(" ^ re ^ ")"

let dlist_of re = "((" ^ re ^ "))"

let bkt_list_of re = "\\[" ^ re ^ "\\]"

let ang_list_of re = "<" ^ re ^ ">"

let astring = "[^\r\n{()%*\"]+"

let dot = "\\."

let sol = "^"

let eol = "$"

let crlf = "\r\n"

let quote_spec_char = "[\\\"]"

let quoted_char = "[^\r\n\\\"]"

let tag = "[^\r\n{()%*\"\\ ]+"

let optional re = (group re) ^ "?"

let orxl l = Core.Std.String.concat l ~sep:"\\|"

let all_of_it re = "^" ^ re ^ "$"

let nz_number = "[1-9][0-9]*"

let number = "[0-9]+"

let space = " "

let qstring = quote ( ( group ( orx quote_spec_char quoted_char ) ) ^ "+" )

(** convert imap mailbox regex to ocaml regex **)
let fixregx_mbox mailbox =
  let str = replace ~regx:"\\*" ~tmpl:".+" mailbox in
  let str = replace ~regx:"^%$" ~tmpl:"^[^/]+$" str in
  let str = replace ~regx:"^%" ~tmpl:"^[^/]+" str in
  replace ~regx:"%$" ~tmpl:"[^/]+$" str

(* date regex *)
let mon = group "Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec"

let dayofweek = group "Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun"

let dd = group ( orx ( group "[0-9]") (group "[0-9][0-9]"))

let dd_fixed = group ( orx ( group " [0-9]") (group "[0-9][0-9]"))

let yyyy = group "[0-9][0-9][0-9][0-9]"

let time = group "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]"

let zone = group "[+-][0-9][0-9][0-9][0-9]"

let date_regex =
  dd ^ "-" ^ mon ^ "-" ^ yyyy

let date_dqregex = quote( date_regex )

let date_time_regex = 
  dd_fixed ^ "-" ^ mon ^ "-" ^ yyyy ^ " " ^ time ^ " " ^ zone

let date_time_dqregex =
  quote( date_time_regex)

(**Date: Mon, 7 Feb 1994 21:52:25 -0800 (PST)**) 
let email_date_regex =
  dd ^ " " ^ mon ^ " " ^ yyyy ^ " " ^ time ^ " " ^ zone

(* Thu, 17 Jul 2014 14:53:00 +0100 (BST) *)
let smtp_date_regex =
  dayofweek ^ ", " ^ email_date_regex

(* append regex *)
let append_regex =
  let cmd = "append" in
  let mbox = group astring in
  let qmbox = group qstring in
  let mailbox = group (orx mbox qmbox) in
  let flags = group (" " ^ list_of astring) in
  let date = group (" " ^ date_time_dqregex) in
  "^" ^ tag ^ " " ^ cmd ^ " " ^ mailbox ^ flags ^ "?" ^ date ^ "? $"

let lappend_regex =
  let cmd = "lappend" in
  let mbox = group astring in
  let qmbox = group qstring in
  let mailbox = group (orx mbox qmbox) in
  let user = group astring in
  let quser = group qstring in
  let user_ = group (orx user quser) in
  "^" ^ tag ^ " " ^ cmd ^ " " ^ mailbox ^ " " ^ user_ ^ "$"
