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
open Regex

exception InvalidSection of string

let header_list = list_of astring (** everything in parenthesis **)

let header_fields_regex = group ("HEADER" ^ dot ^ "FIELDS")

let header_fields_not_regex = group ("HEADER" ^ dot ^ "FIELDS" ^ dot ^ "NOT")

let msgtext_regex = (optional (orxl
["HEADER";header_fields_regex;header_fields_not_regex; "TEXT"]))

let text_regex = all_of_it (group (orxl
["HEADER";header_fields_regex;header_fields_not_regex; "TEXT"; "MIME"]))

let sec_part_regex = (group number) ^ (group ("\\." ^ nz_number)) ^ "*"

let parse_header_list (str:string) : (string list) =
  let str = replace "(" "" str in
  let str = replace ")" "" str in
  Str.split (Str.regexp " ") str

let parse_msgtext (str:string) : (sectionMsgtext option) =
  if match_regex ~case:false str ~regx:(all_of_it ((group header_fields_not_regex) ^ space ^ (group header_list))) then
    let header_list = parse_header_list (Str.matched_group 3 str) in
    Some (HeaderFieldsNot (header_list))
  else if match_regex ~case:false str ~regx:(all_of_it ((group header_fields_regex) ^ space ^ (group header_list))) then
    let header_list = parse_header_list (Str.matched_group 3 str) in
    Some (HeaderFields (header_list))
  else if match_regex ~case:false str ~regx:(all_of_it "HEADER") then
    Some Header
  else if match_regex ~case:false str ~regx:(all_of_it "TEXT") then
    Some Text
  else if str = "" then
    None
  else
    raise (InvalidSection "msgtext")

(** ((( nz-number 1)([. nz-number ]2)* 3)4).(HEADER...5)  **)
let parse_part (str:string) : (sectionPart * string) =
  let part_grp = group sec_part_regex in
  let msg_grp = group "[hmt].*" in
  let opt_msg_grp = optional (dot ^ msg_grp) in
  let rx = all_of_it (part_grp ^ opt_msg_grp) in
  if match_regex ~case:false str ~regx:rx = false then
    raise (InvalidSection ("part:" ^ str))
  else (
    let part = Str.matched_group 1 str in
    let remaining = try Str.matched_group 5 str with _ -> "" in
    let parts = Str.split (Str.regexp "\\.") part in
    let parts = List.fold_left 
    (fun acc item -> List.concat [acc;[int_of_string item]]) [] parts
    in
    (parts,remaining)
  )

(** [section]<postfix>, postfix = nz-number.nz-number **)
let parse_postfix (section:string) : (bodyPart * string) =
  let sect = group "[^]]*" in
  let body_part = (group number) ^ (optional (dot ^ (group nz_number))) in
  let all = all_of_it ((bkt_list_of sect) ^ (optional (ang_list_of (optional body_part)))) in
  if match_regex section ~regx:all then
    let suffix = Str.matched_group 1 section in
    let postfix = try (Str.matched_group 3 section) with _ -> "" in
    let body_part =
    try 
      let l = Str.split (Str.regexp "\\.") postfix in
      List.fold_left (fun acc i->List.concat [acc;[int_of_string i]]) [] l
    with _ -> [] 
    in
    (body_part, suffix)
  else
    raise (InvalidSection "top")

let parse_text (str:string) : (sectionPart * sectionText option) =
  let (part,str) = parse_part str in
  if str = "" then
    (part, None)
  else if match_regex ~case:false str ~regx:(all_of_it "MIME") then
    (part, Some Mime)
  else
    let msg = parse_msgtext str in
    match msg with
    | None -> (part, None)
    | Some msg -> (part, Some (SectionMsgtext msg))

(**  : (sectionSpec,int,int) = **)
let parse_fetch_section (section:string) : (sectionSpec * bodyPart) =
  let section = replace "^[^[]+" "" section in
  let (body_part,section) = parse_postfix section in
  if match_regex ~case:false section ~regx:(sol ^ sec_part_regex) then (
    let (part,text) = parse_text section in
    SectionPart (part,text), body_part
  ) else if match_regex ~case:false section ~regx:(sol ^ msgtext_regex) then (
    let msgtext = parse_msgtext section in
    SectionMsgtext msgtext,body_part
  ) else
    raise (InvalidSection "overall")
