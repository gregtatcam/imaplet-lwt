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

module MapStr = Map.Make(String)

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

let rec args i mbox index labels outdir rand size =
  if i >= Array.length Sys.argv then
    mbox, index,labels, outdir, rand, size
  else
    match Sys.argv.(i) with 
    | "-archive" -> args (i+2) Sys.argv.(i+1) index labels outdir rand size
    | "-index" -> args (i+2) mbox (parse_start_stop Sys.argv.(i+1)) labels outdir rand size
    | "-labels" -> args (i+2) mbox index (parse_labels Sys.argv.(i+1)) outdir rand size
    | "-split" -> args (i+2) mbox index labels (Some Sys.argv.(i+1)) rand size
    | "-rand" -> args (i+1) mbox index labels outdir true size
    | "-size" -> args (i+2) mbox index labels outdir rand 
      (Some ((int_of_string Sys.argv.(i+1)) * 1024 * 1024))
    | _ -> raise InvalidCommand

let usage () =
  Printf.fprintf stderr "usage: get_messages -archive filename -index
  start[:stop] -labels [label1,...,labeln] -split [outdir] -rand -size [maxmsgsizeinMB]\n%!"

let commands f =
  try 
    let mbox,index,labels,outdir,rand,size = args 1 "" (1,max_int) [] None false None in
    if mbox = "" then
      raise InvalidCommand
    else
      try 
        f mbox index labels outdir rand size
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
      let label = Regex.replace ~regx:"\\[Imap\\]/" ~tmpl:"" label in
      let label = Regex.replace ~case:false ~regx:"inbox" ~tmpl:"INBOX" label in
      (label)
  )

let filtered message label labels size = 
  if size <> None && String.length message > Utils.option_value_exn size then
    true
  else if labels <> [] then (
    (List.exists (fun l -> (String.lowercase l) = (String.lowercase label)) labels) = false
  ) else
    false

let output message label outfiles = function
  | None -> Lwt_io.fprintf Lwt_io.stdout "%s%!" message >> return outfiles
  | Some outdir ->
    let label = Regex.replace ~regx:"/" ~tmpl:"." label in
    let label = Regex.replace ~regx:" " ~tmpl:"." label in
    begin
    if MapStr.exists (fun key _ -> label = key) outfiles then
      return (outfiles,MapStr.find label outfiles)
    else (
      Lwt_io.open_file ~mode:Lwt_io.Output 
        (Filename.concat outdir label) >>= fun co ->
      return (MapStr.add label co outfiles,co)
    )
    end >>= fun (outfiles,co) ->
    Lwt_io.fprintf co "%s%!" message >>
    return outfiles

let rec get_random exists size =
  let r = Random.int (size + 1) in
  if MapStr.exists (fun key _ -> (int_of_string key) = r) exists then
    get_random exists size
  else (
    (r, MapStr.add (string_of_int r) r exists)
  )

let fold_email_rand path f init =
  Utils.fold_email_with_file path (fun (cnt,acc) message ->
    Printf.fprintf stderr "message # %d\n%!" cnt;
    return (`Ok (cnt+1,message :: acc))) (1,[]) >>= fun (_,messages) ->
  Random.init (int_of_float (Unix.gettimeofday ()));
  let size = List.length messages in
  let rec loop exists (cnt,acc) =
    if MapStr.cardinal exists = size then
      return (cnt,acc)
    else (
      let (r,exists) = get_random exists size in
      Printf.fprintf stderr "processing random %d %d\n%!" cnt r;
      f acc (List.nth messages r) >>= function
      | `Ok acc -> loop exists (cnt+1,acc)
      | `Done acc -> return (cnt,acc)
    )
  in 
  loop MapStr.empty (1,init) >>= fun (_,acc) ->
  return acc

let get_messages path start stop labels outdir rand size =
  let fold = if rand then fold_email_rand else Utils.fold_email_with_file in
  fold path (fun (cnt,outfiles) message ->
    if cnt > stop then
      return (`Done (cnt,outfiles))
    else (
      let label = mailbox_of_gmail_label message in
      if filtered message label labels size = false then (
        begin
        if cnt >= start then
          output message label outfiles outdir
        else
          return outfiles
        end >>= fun outfiles ->
        return (`Ok (cnt + 1, outfiles))
      ) else
        return (`Ok (cnt, outfiles))
    )
  ) (1,MapStr.empty) >>= fun (_,outfiles) ->
  MapStr.fold (fun _ oc _ -> Lwt_io.close oc) outfiles (return ())

let () =
  commands (fun mbox (start,stop) labels outdir rand size ->
    Lwt_main.run (
      get_messages mbox start stop labels outdir rand size
    )
  )
