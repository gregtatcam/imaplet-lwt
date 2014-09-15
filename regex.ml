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
let match_regex_i case str regx = 
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

let match_regex ?(case=true) ~regx str = 
  let i = match_regex_i case str regx in
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

let bkt_list_of re = "\\[" ^ re ^ "\\]"

let ang_list_of re = "<" ^ re ^ ">"

let astring = "[^\r\n{()%*\"]+"

let dot = "\\."

let sol = "^"

let eol = "$"

let optional re = (group re) ^ "?"

let orxl l = Core.Std.String.concat l ~sep:"\\|"

let all_of_it re = "^" ^ re ^ "$"

let nz_number = "[1-9][0-9]*"

let number = "[0-9]+"

let space = " "

(* date regex *)
let mon = group "Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec"

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
