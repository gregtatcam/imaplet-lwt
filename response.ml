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
open Core.Std
open Imaplet_types

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
    let acc = List.fold l 
      ~init:"" 
      ~f:(fun acc s -> if acc = "" then s else if s = "" then acc else acc ^ Regex.space ^ s) in
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

let write_resp w ?(tag="*") resp =
  let send_wcrlf w str = 
    Lwt_io.write w (str ^ Regex.crlf) >> Lwt_io.flush w
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

let write_resp_untagged writer text =
  write_resp writer (Resp_Untagged text)
