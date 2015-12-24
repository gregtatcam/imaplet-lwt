open Lwt
open Sexplib.Std

exception MultipartNoBoundary

type ctype = 
  [`Audio|`Video|`Image|`Application|`Text|`Multipart|`Message|`Other of string]
  with sexp
type stype = 
  [`Plain|`Rfc822|`Digest|`Alternative|`Parallel|`Mixed|`Other of string] with sexp
type entity = {message:string;offset:int; size:int} with sexp
type boundary = {bopen:string;bclose:string} with sexp
type lightheaders = {position:entity;content_type:ctype; content_subtype:stype; 
  boundary:boundary option;} with sexp
type content = {position:entity;content:
  [`Data|`Message of lightmail |`Multipart of lightmail list]}
and lightmail = {position:entity;headers:lightheaders; content: content} with sexp
type lightmessage = {position:entity;postmark:entity;email:lightmail} with sexp

module MemReader :
sig
  type t
  val of_string : string -> t Lwt.t
  val position : t -> int
  val length : t -> int
  val set_position : t -> int -> unit Lwt.t
  val read_line : t -> string Lwt.t
  val read_line_opt : t -> string option Lwt.t
  val read_char : t -> char Lwt.t
  val last_crlf_size : t -> int Lwt.t
  val message : t -> string
end =
struct
  type t = {message:string;ic:Lwt_io.input_channel;length:int}
  let of_string str =
    let bytes = Lwt_bytes.of_string str in
    let ic = Lwt_io.of_bytes ~mode:Lwt_io.Input bytes in
    Lwt_io.length ic >>= fun length ->
    let length = Int64.to_int length in
    return {message=str;ic;length}
  let length t = t.length
  let position t =
    t.length + (Int64.to_int (Lwt_io.position t.ic))
  let set_position t p =
    Lwt_io.set_position t.ic (Int64.of_int p)
  let read_line t = Lwt_io.read_line t.ic
  let read_line_opt t = Lwt_io.read_line_opt t.ic
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
  let message t = t.message
end

let get_size reader position =
  (MemReader.position reader) - position

let mk_position ?(adj=0) reader offset =
  let size = (get_size reader offset) - adj in
  {message=MemReader.message reader;offset;size}

let adjust_for_boundary reader line_size =
  let current = MemReader.position reader in
  MemReader.last_crlf_size reader >>= fun crlf1 ->
  MemReader.set_position reader (current - (crlf1 + line_size)) >>
  MemReader.last_crlf_size reader >>= fun crlf2 ->
  MemReader.set_position reader current >>
  return (crlf1 + crlf2 + line_size)

module Boundary :
sig
  type t = boundary
  val parse : string -> t option
  val is_open : t option -> string -> bool
  val is_close : t option -> string -> bool
end =
struct
  type t = boundary
  let re_boundary = Re_posix.compile_pat ~opts:[`ICase]
    "boundary=((\"([^\" ]+)\")|([^\" ]+))"
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

module Headers :
sig
  type t = lightheaders
  val parse : ?content_type:ctype -> ?content_subtype:stype -> MemReader.t -> 
    ([`Ok|`Eof] * t) Lwt.t
  val to_string : t -> string
end =
struct
  type t = lightheaders
  let re_content = Re_posix.compile_pat ~opts:[`ICase]
    "^content-type: ([^/ ]+)/([^; ]+)(.*)$"
  let get_type t = match (String.lowercase t) with
    | "audio"->`Audio|"video"->`Video|"image"->`Image|"application"->`Application
    | "text"->`Text|"multipart"->`Multipart|"message"->`Message|t -> `Other t
  let get_subtype s = match (String.lowercase s) with
    |"plain"->`Plain|"rfc822"->`Rfc822|"digest"->`Digest|"alternative"->`Alternative
    |"parallel"->`Parallel|"mixed"->`Mixed|s->`Other s
  let parse ?(content_type=`Text) ?(content_subtype=`Plain) reader =
    let offset = MemReader.position reader in
    let rec read found_content_type boundary_required (header:lightheaders) = 
      MemReader.read_line_opt reader >>= function
      | None -> 
        return (`Eof,{header with position=mk_position reader offset})
      | Some line -> 
        if line = "" then ( (* done parsing, next is body part *)
          if boundary_required && header.boundary = None then
            raise MultipartNoBoundary
          else (
            (* crlf following the headers is not part of the headers and not
             * part of the body either *)
            MemReader.last_crlf_size reader >>= fun adj ->
            let position = mk_position ~adj reader offset in
            return (`Ok, {header with position})
          )
        ) else if found_content_type && boundary_required && header.boundary = None then (
          read found_content_type boundary_required 
            {header with boundary = Boundary.parse line}
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
            read true (content_type = `Multipart) 
              {header with content_type;content_subtype;boundary;}
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
    let position = {message=MemReader.message reader;offset; size = 0} in
    let header =
      {content_type;content_subtype;boundary=None;position;} in
    read false false header
  let to_string (t:lightheaders) =
    String.sub t.position.message t.position.offset t.position.size
end

module rec Content:
sig
  type t = content
  val parse : ?content_type:ctype -> ?content_subtype:stype ->
    ?boundary:boundary -> MemReader.t -> ([`Ok|`Eof|`OkMultipart] * t) Lwt.t
  val to_string: t -> string
end =
struct
  type t = content
  let parse ?(content_type=`Text) ?(content_subtype=`Plain) ?boundary reader =
    let offset = MemReader.position reader in
    let mk_content ?adj reader res content =
      begin
      match adj with
      | None -> return 0
      | Some line ->
        adjust_for_boundary reader (String.length line)
      end >>= fun adj ->
      return (res,{position=mk_position ~adj reader offset;content})
    in
    if content_type = `Multipart then (
      let rec read contents =
        MemReader.read_line_opt reader >>= function
        | None ->
          mk_content reader `Eof (`Multipart (List.rev contents))
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
              let contents = email :: contents in
              match res with
              | `Ok -> consume_multipart contents
              | _ ->
                (* if the last part is a multipart itself then it doesn't
                 * consume the close boundary or more parts, continue parsing
                 * until all parts and close boundary are consumed
                 *)
                match email.content.content with
                | `Multipart _ -> read contents
                | _ ->
                  mk_content reader res (`Multipart (List.rev contents))
            in
            consume_multipart contents
          ) else if Boundary.is_close boundary line then (
            mk_content reader `OkMultipart (`Multipart (List.rev contents))
          ) else
            read contents
      in
      read []
    ) else if content_type = `Message then (
      Email.parse ~content_type ~content_subtype ?boundary 
        reader >>= fun (res,email) ->
      mk_content reader res (`Message email)
    ) else (
      let rec read () =
        MemReader.read_line_opt reader >>= function
        | None -> mk_content reader `Eof `Data
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
              mk_content ~adj:line reader `Ok `Data
            ) else if Boundary.is_close boundary line then (
              mk_content ~adj:line reader `OkMultipart `Data
            ) else
              read ()
      in
      read ()
    )
  let to_string (t:content) =
    String.sub t.position.message t.position.offset t.position.size
end
and 
Email:
sig
  type t = lightmail
  val parse : ?content_type:ctype -> ?content_subtype:stype ->
    ?boundary:boundary -> MemReader.t -> ([`Ok|`Eof|`OkMultipart] * t) Lwt.t
  val to_string : t -> string
  val content : t -> content
end =
struct
  type t = lightmail
  let parse ?(content_type=`Text) ?(content_subtype=`Plain) ?boundary reader =
    let offset = MemReader.position reader in
    Headers.parse ~content_type ~content_subtype reader >>= fun (_,headers) ->
    let boundary =
      match headers.boundary with
      | None -> boundary
      | Some boundary -> Some boundary
    in
    Content.parse ~content_type:headers.content_type
      ~content_subtype:headers.content_subtype ?boundary reader >>= fun (res,content) ->
    (* can't calc the size based on the current reader's position since it includes
     * the boundary, use returned content's position instead, which doesn't
     * include the boundary
     *)
  let size = (content.position.offset + content.position.size) - offset in
  let position = {message = MemReader.message reader; offset; size} in
    return (res,{position;headers;content})
  let to_string (t:lightmail) =
    String.sub t.position.message t.position.offset t.position.size
  let content (t:lightmail) =
    t.content
end

module Postmark :
sig
  type t = entity
  val parse : MemReader.t -> t Lwt.t
  val to_string : t -> string
end = struct
  type t = entity
  let re_postmark = Re_posix.compile_pat ~opts:[`ICase] 
    ("^(From [^ \r\n]+ (mon|tue|wed|thu|fri|sat|sun) " ^
    "(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec) ([^\r\n]+))$")
  let parse reader =
    MemReader.read_line reader >>= fun line ->
    let message = MemReader.message reader in
    if Re.execp re_postmark line then
      return {message;offset = 0; size = String.length line}
    else (
      MemReader.set_position reader 0 >>
      return {message;offset = 0; size = 0}
    )
  let to_string (t:entity) =
    String.sub t.message t.offset t.size
end

(* assume single, complete message *)
module Message :
sig
  type t = lightmessage
  val parse : string -> t Lwt.t
  val to_string : t -> string
end =
struct
  type t = lightmessage
  let parse message =
   MemReader.of_string message >>= fun reader ->
   Postmark.parse reader >>= fun postmark ->
   Email.parse reader >>= fun (res,email) ->
     return {position={message;offset=0;size=String.length message}; postmark; email}
  let to_string (t:lightmessage) =
    String.sub t.position.message t.position.offset t.position.size
end
