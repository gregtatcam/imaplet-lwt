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
open Sexplib
open Sexplib.Conv
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
  let open Parsemail in
  {Mailbox.Message.postmark=Mailbox.Postmark.of_string postmark; 
   Mailbox.Message.email = Email.of_string email}

let make_email_message message =
  let size = String.length message in
  let headers = String.sub message 0 (if size < 1024 * 5 then size else 1024 * 5) in
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
      if Regex.match_regex buff ~regx:"^From: \\([^<]+\\)<\\([^>]+\\)" then
        Str.matched_group 2 buff
      else
        "From daemon@localhost.local"
    in
    let date_time buff =
      let time =
        if Regex.match_regex buff ~regx:"^Date: \\([.]+\\)[\r\n]+" then (
          try 
            Dates.email_to_date_time_exn (Str.matched_group 1 buff)
          with _ -> Dates.ImapTime.now()
        ) else
          Dates.ImapTime.now()
      in
      Dates.postmark_date_time ~time ()
    in
    let post = ("From " ^ (from headers) ^ " " ^ (date_time headers)) in
    (message_of_string post message)
  )

let option_value o ~default = 
  match o with
  | Some v -> v
  | None -> default

let option_value_exn ?(ex=Not_found) = function
  | Some v -> v
  | None -> raise ex

let list_find l f =
  List.exists f l

let list_findi l f =
  let rec findi l i f =
    if i >= List.length l then
      None
    else (
      if f i (List.nth l i) then
        Some (i, List.nth l i)
      else
        findi l (i+1) f
    )
  in
  findi l 0 f

let lock_from_open_flags flags =
  List.fold_left (fun l fl ->
    match fl with
    | Unix.O_RDONLY -> if l = Unix.F_LOCK then l else Unix.F_RLOCK
    | Unix.O_WRONLY|Unix.O_RDWR|Unix.O_APPEND -> Unix.F_LOCK
    | _ -> l
  ) F_RLOCK flags

let _lock lock fd flags = 
  let open Lwt in
  if lock then
    Lwt_unix.lockf fd (lock_from_open_flags flags) 0
  else
    return ()

let _unlock lock fd = 
  let open Lwt in
  if lock then
    Lwt_unix.lockf fd F_ULOCK 0
  else
    return ()

let with_file ?(lock=false) path ~flags ~perms ~mode ~f =
  let open Lwt in
  Lwt_unix.openfile path flags perms >>= fun fd ->
  _lock lock fd flags >>= fun () ->
  let ch = Lwt_io.of_fd ~close:(fun () -> return ()) ~mode fd in
  Lwt.finalize (fun () -> f ch)
  (fun () -> 
    _unlock lock fd >>
    Lwt_io.close ch >> Lwt_unix.close fd
  )

let lines_of_file ?(g=(fun ex -> raise ex)) file ~init ~f =
  Lwt.catch (fun () ->
    let strm = Lwt_io.lines_of_file file in
    Lwt_stream.fold_s (fun line acc ->
      f line acc 
    ) strm init
  ) g

let exists file ?alt tp = 
  let open Lwt in
  catch (fun () ->
  Lwt_unix.stat file >>= fun st ->
  match alt with
  | None -> return (st.Unix.st_kind = tp)
  | Some alt -> return (st.Unix.st_kind = tp || st.Unix.st_kind = alt)
  ) (fun _ -> return false)

let lines str =
  let rec lines_ str l i =
  try
    let index = String.index_from str i '\n' in
    lines_ str (l+1) (index+1)
  with _ -> 
    if i < (String.length str) then 
      l+1 
    else 
      l
  in
  lines_ str 0 0

(* convert the list to a string of sexp *)
let str_sexp_of_list l =
  let sexp = sexp_of_list (fun i -> sexp_of_string i) l in
  Sexp.to_string sexp

(* convert string of sexp to the list *)
let list_of_str_sexp str =
  let sexp = Sexp.of_string str in
  list_of_sexp (fun i -> string_of_sexp i) sexp 

let with_timeout t f g =
  let open Lwt in
  catch (fun () ->
    Lwt_unix.with_timeout t f
  ) (fun ex -> g ex)

(* how to get all interfaces w/out system call?
 * gethostbyname on rasppi returns only hosts content
 *)
let get_interfaces () =
  let open Lwt in
  let strm = Lwt_process.pread_lines ("",[|"ifconfig"|]) in
  let rec read acc =
    Lwt_stream.get strm >>= function
    | Some l ->
      if Regex.match_regex 
      ~regx:"^[ \t]*inet[ \t]+\\([^0-9]+\\)?\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)" l then
        (
          let ip = Str.matched_group 2 l in
          read (ip :: acc)
        )
      else
        read acc
    | None -> return acc
  in
  read []

let postmark = "^from [^ ]+ " ^ Regex.dayofweek ^ " " ^ Regex.mon ^ " " ^
  "[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [0-9]+"

let is_postmark line =
  Regex.match_regex ~case:false ~regx:postmark line

let rec get_message ic buffer f acc =
  let open Lwt in
  Lwt_io.read_line_opt ic >>= function
  | None -> 
    if Buffer.length buffer > 0 then
      let content = Buffer.contents buffer in
      Buffer.clear buffer;
      f acc content
    else
      return acc
  | Some line ->
    let line = line ^ "\n" in
    if is_postmark line && (Buffer.length buffer >0) then (
      let content = Buffer.contents buffer in
      Buffer.clear buffer;
      Buffer.add_string buffer line;
      f acc content >>= fun acc ->
      get_message ic buffer f acc 
    ) else (
      Buffer.add_string buffer line;
      get_message ic buffer f acc
    )

let fold_email_with_file file f init =
  Lwt_io.with_file ~mode:Lwt_io.Input file (fun ic ->
    get_message ic (Buffer.create 100) f init
  )

let fold_email_with_file1 file f init =
  let open Parsemail in
  Lwt_io.with_file ~mode:Lwt_io.Input file (fun ic ->
    get_message ic (Buffer.create 100) (fun acc message ->
      let seq = Mailbox.With_seq.of_string message in
      let message = Mailbox.With_seq.fold_message seq ~f:(fun _ message -> Some message) ~init:None in
      f acc (option_value_exn message)
    ) init
  )

let files_of_directory path f init =
  let open Lwt in
  exists path S_DIR >>= fun res ->
  if res then (
    let strm = Lwt_unix.files_of_directory path in
    Lwt_stream.fold_s (fun file acc ->
      if file <> "." && file <> ".." then (
        f acc file
      ) else
        return acc
    ) strm init >>= fun res ->
    return (`Ok res)
  ) else
    return `NoDir

let parse_user user =
  if Regex.match_regex ~regx:"^\\([^@]+\\)@\\(.+\\)$" user then
    (Str.matched_group 1 user),Some (Str.matched_group 2 user)
  else
    user,None

let user_to_path user =
  let user,domain = parse_user user in
  match domain with
  | None -> user
  | Some domain -> domain ^ "/" ^ user

let user_path ?(regx="%user%") ~path ~user () =
  Regex.replace ~regx ~tmpl:(user_to_path user) path
