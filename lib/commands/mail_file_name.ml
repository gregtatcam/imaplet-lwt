(*
 * Copyright (c) 2013-2016 Gregory Tsipenyuk <gregtsip@cam.ac.uk>
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
open Storage_meta

module MapStr = Map.Make(String)
module MapFlag = Map.Make(
  struct
    type t = mailboxFlags
    let compare f1 f2 = Pervasives.compare f1 f2
  end)

let mail_flags : (string*mailboxFlags) list =
  [ "a", Flags_Keyword "$NotJunk";
  "b", Flags_Keyword "NotJunk";
  "P", Flags_Answered;
  "T", Flags_Deleted;
  "D", Flags_Draft;
  "F", Flags_Flagged;
  "S", Flags_Seen;
  "R", Flags_Recent;]
   
(* get the keyword mapping, hardcoded for now TBD *)
let get_map_to_flag mailbox =
  (*let (>>) map (data,key) = MapStr.add data key map in*)
  List.fold_left (fun acc (m,f) -> MapStr.add m f acc) (MapStr.empty) mail_flags

let get_flag_to_map mailbox =
  List.fold_left (fun acc (m,f) -> MapFlag.add f m acc) (MapFlag.empty) mail_flags

let flags_to_map_str mailbox flags =
  let map = get_flag_to_map mailbox in
  let flags = List.sort Pervasives.compare (List.fold_right (fun f acc -> 
    try (MapFlag.find f map) :: acc with Not_found -> acc
  ) flags []) in 
  String.concat "" flags

let flags_of_map_str mailbox flags =
  let map = get_map_to_flag mailbox in
  let rec fold_right i acc =
    if i = String.length flags then
      acc
    else (
      let acc = try (MapStr.find (String.sub flags i 1) map) :: acc with Not_found -> acc in
      fold_right (i + 1) acc
    )
  in
  fold_right 0 []

(* initial file name secs.rand.host *)
let init_message_file_name internal_date =
  let t = Int64.of_float ((Unix.gettimeofday())*.1000.) in
  let internal_date = 
    Int64.of_float (Dates.ImapTime.to_float internal_date) in
  let host = Unix.gethostname() in
  Random.init (Int64.to_int t);
  let r = Int64.to_string (Random.int64 t) in
  Printf.sprintf "%0Lx.%s.%s.%Ld" t r host internal_date

let init_message_file_name_id id internal_date =
  let internal_date = 
    Int64.of_float (Dates.ImapTime.to_float internal_date) in
  Printf.sprintf "%s.%Ld" id internal_date

let make_file_name file mailbox metadata =
  Printf.sprintf "%s,S=%d,M=%s:2,%s" file metadata.size 
    (Int64.to_string metadata.modseq) (flags_to_map_str mailbox metadata.flags)

(* get the filename containing the message *)
let make_message_file_name ?(init_file=init_message_file_name) mailbox metadata =
  let file = init_file metadata.internal_date in
  make_file_name file mailbox metadata

(* time.rand.host.internal_date,S(size)=..,M(modseq)=..:2,[flags] *)
(* 1415570721.20f64da12ed.129054952358.dhcp-172-17-153-93.eduroam.wireless.private.cam.ac.uk,S=2514,M=0:2,Sa *)
let message_file_name_to_data mailbox file =
  let _ = Regex.match_regex
    ~regx:"\\([^,\\.]+\\),S=\\([0-9]+\\),M=\\([0-9]+\\):2,\\(.*\\)$"
    file 
  in
  let internal = Dates.ImapTime.of_float (float_of_string (Str.matched_group 1 file) ) in
  let size = int_of_string (Str.matched_group 2 file) in
  let modseq = Int64.of_string (Str.matched_group 3 file) in
  let flags = flags_of_map_str mailbox (Str.matched_group 4 file) in
  (internal,size,modseq,flags)

(* update modeseq and flags *)
let update_message_file_name mailbox file metadata =
  let (_,size,modseq,flags) = message_file_name_to_data mailbox file in
  let _ = Regex.match_regex ~regx:"^\\([^,]+\\)" file in
  let immute = Str.matched_group 1 file in
  Printf.sprintf "%s,S=%d,M=%s:2,%s" immute size 
    (Int64.to_string metadata.modseq) (flags_to_map_str mailbox metadata.flags)

