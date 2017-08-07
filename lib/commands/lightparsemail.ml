(*
 * Copyright (c) 2015-2016 Gregory Tsipenyuk <gregtsip@cam.ac.uk>
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
open Sexplib
open Sexplib.Std

exception MultipartNoBoundary

module MapStr = Map.Make(String)

type ctype = 
  [`Audio|`Video|`Image|`Application|`Text|`Multipart|`Message|`Other of string]
  [@@deriving sexp]
type stype = 
  [`Plain|`Rfc822|`Digest|`Alternative|`Parallel|`Mixed|`Other of string]
  [@@deriving sexp]
type entity = {offset:int; size:int;lines:int} [@@deriving sexp]
type boundary = {bopen:string;bclose:string} [@@deriving sexp]
type lightheaders = {position:entity;content_type:ctype; content_subtype:stype; 
boundary:boundary option;} [@@deriving sexp]
type lightcontent_ = [`Data|`Message of lightmail |`Multipart of lightmail list]
and lightcontent = {position:entity;content:lightcontent_}
and lightmail = {position:entity;headers:lightheaders; body: lightcontent}
[@@deriving sexp]
type lightmessage = {position:entity;postmark:entity;email:lightmail}
[@@deriving sexp]

let empty_position = {offset=0;size=0;lines=0}
let empty_headers =
  {position=empty_position;content_type=`Text;content_subtype=`Plain;boundary=None}
let empty_content = {position=empty_position;content=`Data}
let empty_email =
  {position=empty_position;headers=empty_headers;body=empty_content}

(* parse text stream (into lines for now) *)
module TextStream :
sig
  type t
  val of_string : string -> t
  val read_line_opt : t -> string option
end =
struct
  let re_line = Re_posix.compile_pat ~opts:[`Newline] "^([^\r\n]*)([\r\n]*)"

  type t = {text: string; offset: int ref}

  let of_string text = {text; offset = ref 0}

  let read_line_opt t = 
    try
      let subs = Re.exec ~pos:!(t.offset) re_line t.text in
      let line = Re.get subs 1 in
      let (ofs,len) = 
        try
          Re.get_ofs subs 2
        with Not_found -> 
          Re.get_ofs subs 1
      in  
      t.offset := len;
      Some line
    with Not_found -> None
end

(* wrapper for Lwt_io.read* methods to read in-memory bytes *)
module MemReader :
sig
  type t
  (* create instance from the string *)
  val of_string : string -> t Lwt.t
  (* get current position *)
  val position : t -> int
  (* get stream length *)
  val length : t -> int
  (* set stream position *)
  val set_position : t -> int -> unit Lwt.t
  (* read line *)
  val read_line : t -> string Lwt.t
  (* read line *)
  val read_line_opt : t -> string option Lwt.t
  (* read character *)
  val read_char : t -> char Lwt.t
  (* get the size of the last crlf *)
  val last_crlf_size : t -> int Lwt.t
  (* get underlying message *)
  val to_string : t -> string
  (* get consumed lines *)
  val lines : t -> int
end =
struct
  type t = {message:string;ic:Lwt_io.input_channel;length:int;lines:int ref}

  let of_string str =
    let bytes = Lwt_bytes.of_string str in
    let ic = Lwt_io.of_bytes ~mode:Lwt_io.Input bytes in
    Lwt_io.length ic >>= fun length ->
    let length = Int64.to_int length in
    return {message=str;ic;length;lines = ref 0}

  let length t = t.length

  let position t =
    t.length + (Int64.to_int (Lwt_io.position t.ic))

  let set_position t p =
    Lwt_io.set_position t.ic (Int64.of_int p)

  let read_line t = 
    t.lines := !(t.lines) + 1;
    Lwt_io.read_line t.ic

  let read_line_opt t = 
    Lwt_io.read_line_opt t.ic >>= function
    | None -> return None
    | Some line ->
      t.lines := !(t.lines) + 1;
      return (Some line)

  let read_char t = Lwt_io.read_char t.ic

  let last_crlf_size t =
    let current = position t in
    let size cnt =
      set_position t current >>
      return cnt
    in
    let rec rewind prev_cr prev_nl cnt =
      set_position t (current - cnt) >>
      read_char t >>= function
      | '\r' -> 
        if prev_cr && prev_nl then
          size (cnt - 1)
        else
          rewind true prev_nl (cnt + 1)
      | '\n' -> 
        if prev_nl then
          size (cnt - 1)
        else
          rewind prev_cr true (cnt + 1)
      | _ -> size (cnt - 1)
    in
    rewind false false 1

  let to_string t = t.message

  let lines t = !(t.lines)
end

(* get the size for the given position to the current position *)
let get_size reader position =
  (MemReader.position reader) - position

(* create entity instance *)
let mk_position ?(adj=0) reader offset ~lines =
  let size = (get_size reader offset) - adj in
  {offset;size;lines}

(* get the size of the boundary + preceding and following crlf *)
let adjust_for_boundary reader line_size =
  let current = MemReader.position reader in
  MemReader.last_crlf_size reader >>= fun crlf1 ->
  MemReader.set_position reader (current - (crlf1 + line_size)) >>
  MemReader.last_crlf_size reader >>= fun crlf2 ->
  MemReader.set_position reader current >>
  return (crlf1 + crlf2 + line_size)

(* define operation on Boundary *)
module Boundary :
sig
  type t = boundary
  (* parse the boundary *)
  val parse : string -> t option
  (* is open boundary *)
  val is_open : t option -> string -> bool
  (* is close boundary *)
  val is_close : t option -> string -> bool
end =
struct
  type t = boundary

  let re_boundary = Re_posix.compile_pat ~opts:[`ICase]
    "boundary=((\"([^\"\r\n]+)\")|([^\"\r\n ]+))"

  let parse line =
    try
      let subs = Re.exec re_boundary line in
      let boundary = Re.get subs 1 in
      let boundary = 
        if String.get boundary 0 = '"' then Re.get subs 3 else boundary in
      let bopen = "--" ^ boundary in
      let bclose = bopen ^ "--" in
      Some {bopen;bclose}
    with Not_found -> None

  let is_open (t:boundary option) line =
    match t with
    |Some b ->
      (String.trim line) = b.bopen
    | None -> false

  let is_close t line =
    match t with
    | Some b ->
      (String.trim line) = b.bclose
    | None -> false
end

(* email headers *)
module Headers :
sig
  type t = {message:string;headers_:lightheaders}
  (* create type *)
  val create : string -> lightheaders -> t
  (* parse headers *)
  val parse : ?content_type:ctype -> ?content_subtype:stype -> MemReader.t -> 
    ([`Ok|`Eof] * t) Lwt.t
  (* get headers as the string *)
  val to_string : t -> string
  (* get headers as the list *)
  val to_list : t -> (string * string ) list
  (* get headers as the map *)
  val to_map : t -> string Map.Make(String).t
end =
struct
  type t = {message:string;headers_:lightheaders}

  let create message headers_ = {message;headers_}

  let re_content = Re_posix.compile_pat ~opts:[`ICase]
    "^content-type: ([^/ ]+)/([^; ]+)(.*)$"

  let get_type t = match (String.lowercase t) with
    | "audio"->`Audio|"video"->`Video|"image"->`Image|"application"->`Application
    | "text"->`Text|"multipart"->`Multipart|"message"->`Message|t -> `Other t

  let get_subtype s = match (String.lowercase s) with
    |"plain"->`Plain|"rfc822"->`Rfc822|"digest"->`Digest|"alternative"->`Alternative
    |"parallel"->`Parallel|"mixed"->`Mixed|s->`Other s

  let parse ?(content_type=`Text) ?(content_subtype=`Plain) reader =
    let message = MemReader.to_string reader in
    let offset = MemReader.position reader in
    let lines = MemReader.lines reader in
    let rec read found_content_type boundary_required (header:lightheaders) = 
      MemReader.read_line_opt reader >>= function
      | None -> 
        let lines = MemReader.lines reader - lines in
        return (`Eof,{message;headers_={header with position=mk_position reader offset ~lines}})
      | Some line -> 
        if line = "" then ( (* done parsing, next is body part *)
          if boundary_required && header.boundary = None then
            raise MultipartNoBoundary
          else (
            (* crlf following the headers is not part of the headers and not
             * part of the body either *)
            MemReader.last_crlf_size reader >>= fun adj ->
            let lines = MemReader.lines reader - lines - 1 in
            let position = mk_position ~adj reader offset ~lines in
            return (`Ok, {message;headers_={header with position}})
          )
        ) else if found_content_type && boundary_required && header.boundary = None then (
          read found_content_type boundary_required {header with boundary = Boundary.parse line}
        ) else if found_content_type = false then (
          catch (fun () ->
            let subs = Re.exec re_content line in
            let content_type = get_type (Re.get subs 1) in
            let content_subtype = get_subtype (Re.get subs 2) in
            let boundary = 
              if content_type = `Multipart then 
                Boundary.parse (Re.get subs 3)
              else 
                None
            in
            read true (content_type = `Multipart) {header with content_type;content_subtype;boundary;}
          )(function Not_found -> read found_content_type boundary_required header
            | ex -> raise ex)
        ) else
          read found_content_type boundary_required header 
    in
    let content_type,content_subtype = 
      if content_type = `Multipart && content_subtype = `Digest then
        `Message, `Rfc822
      else
        content_type,content_subtype
    in
    let position = {offset; size = 0; lines=0} in
    let header =
      {content_type;content_subtype;boundary=None;position;} in
    read false false header

  let to_string t =
    String.sub t.message t.headers_.position.offset t.headers_.position.size

  let get_headers t cb init =
    let re = Re_posix.compile_pat "^([^\t :]+):[ ]*(.*)$" in
    let ts = TextStream.of_string (to_string t) in
    let rec read name value acc =
      match TextStream.read_line_opt ts with
      | None -> (name,value,acc)
      | Some line ->
        try
          let subs = Re.exec re line in
          let acc = 
            (* call with last consume header/value. new header means all 
             * (if any) values with FWS are consumed
             *)
            if name <> "" then
              cb acc name value
            else
              acc
          in
          let name = Re.get subs 1 in
          let value = Re.get subs 2 in
          read name value acc
        with Not_found -> (* must be FWS *)
          let value = 
            if name <> "" then 
              String.concat "\n" [value; line] 
            else 
              value 
          in
          read name value acc
    in
    let (name,value,acc) = read "" "" init in
    let acc =
      if name <> "" then
        cb acc name value
      else
        acc
    in
    acc

  let to_list t =
    List.rev (get_headers t (fun acc name value ->
      (name,value) :: acc) [])

  let to_map t =
    get_headers t (fun acc name value ->
      MapStr.add name value acc
    ) MapStr.empty
end

(* define body content part *)
module rec Content:
sig
  type t = {message:string;content_:lightcontent}
  (* create type *)
  val create : string -> lightcontent -> t
  (* parse content *)
  val parse : ?content_type:ctype -> ?content_subtype:stype ->
    ?boundary:boundary -> MemReader.t -> ([`Ok|`Eof|`OkMultipart] * t) Lwt.t
  (* get content string *)
  val to_string: t -> string
  (* get content *)
  val content : t -> [`Data of Content.t|`Message of Email.t|`Multipart of Email.t list]
end =
struct
  type t = {message:string;content_:lightcontent}

  let create message content_ = {message;content_}

  let parse ?(content_type=`Text) ?(content_subtype=`Plain) ?boundary reader =
    let message = MemReader.to_string reader in
    let offset = MemReader.position reader in
    let mk_content ?adj reader res lines content =
      begin
      match adj with
      | None -> return 0
      | Some line ->
        adjust_for_boundary reader (String.length line)
      end >>= fun adj ->
      return (res,{message;content_={position=mk_position ~adj reader offset ~lines;content}})
    in
    if content_type = `Multipart then (
      let lines = MemReader.lines reader in
      let rec read contents =
        MemReader.read_line_opt reader >>= function
        | None ->
          mk_content reader `Eof lines (`Multipart (List.rev contents))
        | Some line -> 
          if Boundary.is_open boundary line then (
            (* consume all parts consisting of header (optional) and content the
             * boundary token delimets the part, the close boundary completes
             * multipart parsing; content in the multipart is reponsible for
             * consuming it's delimeter (end), exception is the last part which
             * is also multipart
             *)
            let rec consume_multipart contents =
              Email.parse ~content_type ~content_subtype ?boundary 
                reader >>= fun (res,email) ->
              let contents = email.Email.mail_ :: contents in
              match res with
              | `Ok -> consume_multipart contents
              | _ ->
                (* if the last part is a multipart itself then it doesn't
                 * consume the close boundary or more parts, continue parsing
                 * until all parts and close boundary are consumed
                 *)
                match email.Email.mail_.body.content with
                | `Multipart _ -> read contents
                | _ ->
                  mk_content reader res (MemReader.lines reader - lines) 
                    (`Multipart (List.rev contents))
            in
            consume_multipart contents
          ) else if Boundary.is_close boundary line then (
            mk_content reader `OkMultipart (MemReader.lines reader - lines)
              (`Multipart (List.rev contents))
          ) else
            read contents
      in
      read []
    ) else if content_type = `Message then (
      let lines = MemReader.lines reader in
      Email.parse ~content_type ~content_subtype ?boundary 
        reader >>= fun (res,email) ->
      let lines = MemReader.lines reader - lines in
      mk_content reader res lines (`Message email.Email.mail_)
    ) else (
      let lines = MemReader.lines reader in
      let rec read () =
        MemReader.read_line_opt reader >>= function
        | None -> mk_content reader `Eof (MemReader.lines reader - lines) `Data
        | Some line ->
          match boundary with
          | None -> read ()
          | Some _ -> 
            (* if part of multipart then consuming the boundary
             * indicates the end of the part, but the boundary is
             * not included in the part, so need to adjust the size by
             * the length of the line and preceding crlf which belongs to the
             * boundary
             *)
            if Boundary.is_open boundary line then (
              let lines = MemReader.lines reader - lines - 2 in
              mk_content ~adj:line reader `Ok lines `Data
            ) else if Boundary.is_close boundary line then (
              let lines = MemReader.lines reader - lines - 2 in
              mk_content ~adj:line reader `OkMultipart lines `Data
            ) else
              read ()
      in
      read ()
    )

  let to_string t =
    String.sub t.message t.content_.position.offset t.content_.position.size

  let content t = 
    match t.content_.content with
    | `Data -> `Data (Content.create t.message t.content_)
    | `Message email -> `Message (Email.create t.message email)
    | `Multipart lemail -> 
      `Multipart (List.map (fun email -> Email.create t.message email) lemail)
end
and 
(* define email *)
Email:
sig
  type t = {message:string;mail_:lightmail}
  (* create type *)
  val create : string -> lightmail -> t
  (* empty *)
  val empty : unit -> t
  (* parse email *)
  val parse : ?content_type:ctype -> ?content_subtype:stype ->
    ?boundary:boundary -> MemReader.t -> ([`Ok|`Eof|`OkMultipart] * t) Lwt.t
  (* get email string headers+body *)
  val to_string : t -> string
  (* get body *)
  val body : t -> Content.t
  (* get headers *)
  val headers : t -> Headers.t
  (* get content *)
  val content : t -> lightcontent_
end =
struct
  type t = {message:string;mail_:lightmail}

  let create message mail_ = {message;mail_}

  let empty () = {message="";mail_=empty_email}

  let parse ?(content_type=`Text) ?(content_subtype=`Plain) ?boundary reader =
    let message = MemReader.to_string reader in
    let offset = MemReader.position reader in
    let lines = MemReader.lines reader in
    Headers.parse ~content_type ~content_subtype reader >>= fun (_,headers) ->
    let boundary =
      match headers.Headers.headers_.boundary with
      | None -> boundary
      | Some boundary -> Some boundary
    in
    Content.parse ~content_type:headers.Headers.headers_.content_type
      ~content_subtype:headers.Headers.headers_.content_subtype 
      ?boundary reader >>= fun (res,content) ->
    (* can't calc the size based on the current reader's position since it includes
     * the boundary, use returned content's position instead, which doesn't
     * include the boundary
     *)
    let size = (content.Content.content_.position.offset + 
      content.Content.content_.position.size) - offset in
    let lines = MemReader.lines reader - lines in
    let position = {offset; size; lines} in
    return (res,{message;
      mail_={position;headers=headers.Headers.headers_;body=content.Content.content_}})

  let to_string t =
    String.sub t.message t.mail_.position.offset t.mail_.position.size

  let body t =
    Content.create t.message t.mail_.body

  let headers t =
    Headers.create t.message t.mail_.headers

  let content t =
    t.mail_.body.content
end

(* define postmark *)
module Postmark :
sig
  type t = {message:string;postmark_:entity}
  (* create type *)
  val create : string -> entity -> t
  (* parse postmark *)
  val parse : MemReader.t -> t Lwt.t
  (* get postmark as string *)
  val to_string : t -> string
end = struct
  type t = {message:string;postmark_:entity}

  let create message postmark_ = {message;postmark_}

  let re_postmark = Re_posix.compile_pat ~opts:[`ICase] 
    ("^(From [^ \r\n]+ (mon|tue|wed|thu|fri|sat|sun) " ^
    "(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec) ([^\r\n]+))$")

  let parse reader =
    let message = MemReader.to_string reader in
    MemReader.read_line reader >>= fun line ->
    if Re.execp re_postmark line then
      return {message;postmark_={offset = 0; size = String.length line; lines = 1}}
    else (
      MemReader.set_position reader 0 >>
      return {message;postmark_={offset = 0; size = 0; lines = 0}}
    )

  let to_string t =
    String.sub t.message t.postmark_.offset t.postmark_.size
end

(* define message postmark+email *)
module Message :
sig
  type t = {message:string;message_:lightmessage}
  (* create type *)
  val create : string -> lightmessage -> t
  (* parse message *)
  val parse : string -> t Lwt.t
  (* get message as string *)
  val to_string : t -> string
  (* get postmark *)
  val postmark : t -> Postmark.t
  (* get email *)
  val email : t -> Email.t
  (* get light message *)
  val message : t -> lightmessage
  (* parse message, get string of sexp of the message, and concat
   * the size of sexp-string, sexp-string, and message
   *)
  val to_parsed_message_with_header : string -> string Lwt.t
  (* reverse of the above *)
  val from_parsed_message_with_header : string -> t
end =
struct
  type t = {message:string;message_:lightmessage}

  let create message message_ = {message;message_}

  let parse message =
    MemReader.of_string message >>= fun reader ->
    let lines = MemReader.lines reader in
    Postmark.parse reader >>= fun postmark ->
    Email.parse reader >>= fun (res,email) ->
    let lines = MemReader.lines reader - lines in
    return {message;message_={position={offset=0;size=String.length message;lines}; 
      postmark=postmark.Postmark.postmark_; email=email.Email.mail_}}

  let to_string t =
    String.sub t.message t.message_.position.offset t.message_.position.size

  let postmark t =
    Postmark.create t.message t.message_.postmark

  let email t =
    Email.create t.message t.message_.email

  let message t = t.message_

  let to_parsed_message_with_header message =
    parse message >>= fun light_message ->
    let sexp_str = Sexp.to_string (sexp_of_lightmessage light_message.message_) in
    let header = Printf.sprintf "%06d%s" (String.length sexp_str) sexp_str in
    return (String.concat "" [header;message])

  let from_parsed_message_with_header message =
    let size = int_of_string (String.sub message 0 6) in
    let sexp_str = String.sub message 6 size in
    let size = size + 6 in
    let light_message = lightmessage_of_sexp (Sexp.of_string sexp_str) in
    create (String.sub message size (String.length message - size)) light_message
end
