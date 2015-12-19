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
open Lwt

module StatusResponse : sig
  type response_type = Ok|Bad|No|Preauth|Bye
  val ok : ?tag:string -> ?code:responseCode option -> string -> string
  val bad : ?tag:string -> ?code:responseCode  option -> string -> string
  val no : ?tag:string -> ?code:responseCode  option -> string -> string
  val preauth : ?code:responseCode  option -> string -> string
  val bye : ?code:responseCode  option -> string -> string
  val untagged : string -> string
  val any : string -> string
  val continue : ?text:string -> unit -> string
end = struct
  type response_type = Ok|Bad|No|Preauth|Bye

  (** use for utf8?? **)
  let to_str x = x

  (** have to change the brackets, review neew to build
   * structures that then are printed, maybe sexp TBD
   **)
  let response_code code = match code with
    | Some rc -> (match rc with
      | RespCode_Alert -> "[ALERT"
      | RespCode_Badcharset ->  "[BADCHARSET"
      | RespCode_Capability ->  "[CAPABILITY"
      | RespCode_Parse ->  "[PARSE"
      | RespCode_Permanentflags ->  "[PERMANENTFLAGS"
      | RespCode_Read_only ->  "[READ-ONLY"
      | RespCode_Read_write ->  "[READ-WRITE"
      | RespCode_Trycreate ->  "[TRYCREATE"
      | RespCode_Uidnext ->  "[UIDNEXT"
      | RespCode_Uidvalidity ->  "[UIDVALIDITY"
      | RespCode_Unseen ->  "[UNSEEN"
      | RespCode_Highestmodseq ->  "[HIGHESTMODSEQ")
    | None ->  ""

  let get_rtype = function
    | Ok ->  "OK"
    | Bad ->  "BAD"
    | No ->  "NO"
    | Preauth ->  "PREAUTH"
    | Bye ->  "BYE"

  let get_response ?(tag="*") ?(code=None) ~rtype text =
    let l = [tag; get_rtype rtype; response_code code; text] in
    let acc = List.fold_left  
      (fun acc s -> if acc = "" then s else if s = "" then acc else acc ^ Regex.space ^ s) "" l in
    match code with 
    |None-> to_str acc
    |Some _ -> to_str (acc ^ "]")

  let ok ?(tag="*") ?(code=None) text = get_response ~tag ~code ~rtype:Ok text

  let bad ?(tag="*") ?(code=None) text = get_response ~tag ~code ~rtype:Bad text

  let no ?(tag="*") ?(code=None) text = get_response ~tag ~code ~rtype:No text

  let preauth ?(code=None) text = get_response ~tag:"*" ~code ~rtype:Preauth text

  let bye ?(code=None) text = get_response ~tag:"*" ~code ~rtype:Bye text

  let untagged text = to_str ("*" ^ Regex.space ^ text)

  let any text = to_str text

  let continue ?text () = 
    let pl = "+" in
    let str = (match text with 
      | None -> pl
      | Some t -> pl ^ Regex.space ^ t) in
    to_str str
end

let buff_size = 4096

let get_pr str =
  let str_len = String.length str in
  if str_len >= 40 then (
    let head = String.sub str 0 40 in
    let tail = String.sub str (str_len - 40) 40 in
    head ^ "..." ^ tail
  ) else
    str

let write_compressed_block w strm buff_in offset_in len_in buff_out len_out =
  let (fi,used_in,used_out) = Zlib.deflate strm buff_in offset_in len_in
      buff_out 0 len_out Zlib.Z_SYNC_FLUSH in
  Log_.log `Info2 (Printf.sprintf " -- writing compressed data %b %d %d %d %d\n" 
      fi used_in used_out offset_in len_in);
  Lwt_io.write w (String.sub buff_out 0 used_out) >>
  Lwt_io.flush w >>
  return (fi,used_in,used_out)

let write_compressed w strm resp =
  let len_resp = String.length resp in
    Log_.log `Info2 (Printf.sprintf "--> writing compressed data:start %d\n" len_resp); 
  Log_.log `Info2 (Printf.sprintf "--> un-compressed data:start %s$$$$\n%!" (get_pr resp));
  let buffout = String.create buff_size in
  let rec _compress offset len =
    write_compressed_block w strm resp offset len
      buffout buff_size >>= fun (fi,used_in,used_out) ->
    let offset = offset + used_in in
    let len = len - used_in in
    if len = 0 then (
      Log_.log `Info2 (Printf.sprintf "<-- compression complete %d %d\n" offset len); 
      return ()
    ) else
      _compress offset len
  in
  _compress 0 len_resp

let write_resp compress id w ?(tag="*") resp =
  let send_wcrlf w str = 
    Log_.log `Info3 (Printf.sprintf "<-- %s: %s\n" (Int64.to_string id) str);
    match compress with
    | None -> Lwt_io.write w (str ^ Regex.crlf)
    | Some (_,strm,_,_) -> write_compressed w strm (str ^ Regex.crlf)
  in
  match resp with
  | Resp_Ok (code, s) -> send_wcrlf w (StatusResponse.ok ~tag ~code s)
  | Resp_No (code, s) -> send_wcrlf w (StatusResponse.no ~tag ~code s)
  | Resp_Bad (code, s) -> send_wcrlf w (StatusResponse.bad ~tag ~code s)
  | Resp_Bye (code, s) -> send_wcrlf w (StatusResponse.bye ~code s)
  | Resp_Preauth (code, s) -> send_wcrlf w (StatusResponse.preauth ~code s)
  | Resp_Cont (text) -> send_wcrlf w (StatusResponse.continue ~text ())
  | Resp_Untagged (text) -> send_wcrlf w (StatusResponse.untagged text)
  | Resp_Any (text) -> send_wcrlf w (StatusResponse.any text)

let write_resp_untagged compress id writer text =
  write_resp compress id writer (Resp_Untagged text)

let write_resp_untagged_vector compress id w resp =
  let l = List.concat [["* "];resp;[Regex.crlf]] in
  match compress with
  | Some (_,strm,_,_) ->
    let buff = String.concat "" l in
    write_compressed w strm buff
  | None ->
    Lwt_list.iter_s (Lwt_io.write w) l
