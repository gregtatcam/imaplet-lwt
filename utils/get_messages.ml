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
open Lwt
open Imaplet
open Commands

exception InvalidCommand

let parse_start_stop str =
  let _ = Regex.match_regex str ~regx:"\\([0-9]+\\)\\(:\\([0-9]+\\|*\\)\\)?" in
  let start = int_of_string (Str.matched_group 1 str) in
  let stop = 
    try
      let stop = Str.matched_group 3 str in
      if stop = "*" then
        max_int
      else
        int_of_string stop
    with _ -> start
  in
  (start,stop)

let parse_labels str = 
  let labels = Str.split (Str.regexp ",") str in
  List.fold_left (fun acc l -> (Regex.dequote l) :: acc) [] labels

let rec args i mbox index labels =
  if i >= Array.length Sys.argv then
    mbox, index,labels
  else
    match Sys.argv.(i) with 
    | "-archive" -> args (i+2) Sys.argv.(i+1) index labels
    | "-index" -> args (i+2) mbox (parse_start_stop Sys.argv.(i+1)) labels
    | "-labels" -> args (i+2) mbox index (parse_labels Sys.argv.(i+1))
    | _ -> raise InvalidCommand

let usage () =
  Printf.fprintf stderr "usage: get_messages -archive filename -index start[:stop] -folders [folder1,folder2]\n%!"

let commands f =
  try 
    let mbox,index,labels = args 1 "" (1,max_int) [] in
    if mbox = "" then
      raise InvalidCommand
    else
      try 
        f mbox index labels
      with ex -> Printf.printf "%s\n%!" (Printexc.to_string ex)
  with _ -> usage ()

let mailbox_of_gmail_label message = 
  if Regex.match_regex message ~regx:"^X-Gmail-Labels: \\(.+\\)$" = false then 
    ("INBOX")
  else (
    let labels = Str.split (Str.regexp ",") (Str.matched_group 1 message) in
    let (label,inferred) =
    List.fold_left (fun (label,inferred) l ->
      match l with 
      | "Important" -> (label,inferred)
      | "Starred" -> (label,inferred)
      | "Sent" -> (label,(Some "Sent Messages"))
      | "Trash" -> (label,(Some "Deleted Messages"))
      | "Unread" -> (label,inferred)
      | "Draft" -> (label,(Some "Drafts"))
      | label -> ((Some label),inferred)
    ) (None,None) labels
    in
    match label with
    | None ->
      begin
      match inferred with
      | None -> ("INBOX")
      | Some label -> (label)
      end
    | Some label ->
      let label = Regex.replace ~regx:"[Imap]/" ~tmpl:"" label in
      let label = Regex.replace ~case:false ~regx:"inbox" ~tmpl:"INBOX" label in
      (label)
  )

let filtered message labels = 
  if labels = [] then
    false
  else (
    let label = mailbox_of_gmail_label message in
    (List.exists (fun l -> (String.lowercase l) = (String.lowercase label)) labels) = false
  )

let get_messages path start stop labels =
  Lwt_io.with_file ~mode:Lwt_io.Input path (fun ic ->
    let buffer = Buffer.create 10000 in
    let rec loop ic buffer cnt = 
      let read ic cnt stop =
        if cnt > stop then
          return None
        else
          Lwt_io.read_line_opt ic 
      in
      read ic cnt stop >>= function
      | Some line ->
      let line = line ^ "\n" in
      let regx = "^from [^ ]+ " ^ Regex.dayofweek ^ " " ^ Regex.mon ^ " " ^
        "[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [0-9]+" in
      if Regex.match_regex line ~case:false ~regx && Buffer.length buffer > 0 then (
        let message = Buffer.contents buffer in
        let counting = 
        if filtered message labels = false then (
          if cnt >= start then
            Printf.printf "%s%!" message;
          true
        ) else
          false
        in
        Buffer.clear buffer;
        Buffer.add_string buffer line;
        loop ic buffer (if counting then (cnt+1) else cnt)
      ) else (
        Buffer.add_string buffer line;
        loop ic buffer cnt
      )
      | None -> return ()
    in
    loop ic buffer 1 
  )

let () =
  commands (fun mbox (start,stop) labels ->
    Lwt_main.run (
      get_messages mbox start stop labels
    )
  )
