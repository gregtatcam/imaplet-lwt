open Lwt

exception MultipartNoBoundary

type ctype = 
  [`Audio|`Video|`Image|`Application|`Text|`Multipart|`Message|`Other of string] 
type stype = 
  [`Plain|`Rfc822|`Digest|`Alternative|`Parallel|`Mixed|`Other of string]
type entity = {offset:int; size:int}
type boundary = {bopen:string;bclose:string}
type lightheaders = {position:entity;content_type:ctype; content_subtype:stype; 
  boundary:boundary option;}
type content = {position:entity;content:
  [`Data|`Message of lightmail |`Multipart of lightmail list]}
and lightmail = {position:entity;headers:lightheaders; content: content}
type lightmessage = {position:entity;postmark:entity;email:lightmail}

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
end =
struct
  type t = {ic:Lwt_io.input_channel;length:int}
  let of_string str =
    let bytes = Lwt_bytes.of_string str in
    let ic = Lwt_io.of_bytes ~mode:Lwt_io.Input bytes in
    Lwt_io.length ic >>= fun length ->
    let length = Int64.to_int length in
    return {ic;length}
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
end

let get_size reader position =
  (MemReader.position reader) - position

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
end =
struct
  type t = lightheaders
  let re_content = Re_posix.compile_pat ~opts:[`ICase]
    "^content-type: ([^/ ]+)/([^; ]+)(.*)$"
  let get_type t = match t with
    | "audio"->`Audio|"video"->`Video|"image"->`Image|"application"->`Application
    | "text"->`Text|"multipart"->`Multipart|"message"->`Message|t -> `Other t
  let get_subtype s = match s with
    |"plain"->`Plain|"rfc822"->`Rfc822|"digest"->`Digest|"alternative"->`Alternative
    |"parallel"->`Parallel|"mixed"->`Mixed|s->`Other s
  let parse ?(content_type=`Text) ?(content_subtype=`Plain) reader =
    let rec read found_content_type boundary_required (header:lightheaders) = 
      MemReader.read_line_opt reader >>= function
      | None -> 
        return (`Eof,{header with position=
          {header.position with size = get_size reader header.position.offset}})
      | Some line -> 
        if line = "" then (
          if boundary_required && header.boundary = None then
            raise MultipartNoBoundary
          else
            return (`Ok, {header with position=
              {header.position with size = get_size reader header.position.offset}})
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
              {header with content_type;content_subtype;boundary}
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
    let position = {offset = MemReader.position reader; size = 0} in
    let header = {content_type;content_subtype;boundary=None;position} in
    read false false header
end

module rec Content:
sig
  type t = content
  val parse : ?content_type:ctype -> ?content_subtype:stype ->
    ?boundary:boundary -> MemReader.t -> ([`Ok|`Eof|`OkMultipart] * t) Lwt.t
end =
struct
  type t = content
  let parse ?(content_type=`Text) ?(content_subtype=`Plain) ?boundary reader =
    let position = {offset=MemReader.position reader;size=0} in
    let mk_content ?(adj=0) content =
      {position={position with size = (get_size reader position.offset)-adj};content} in
    if content_type = `Multipart then (
      let rec read contents =
        MemReader.read_line_opt reader >>= function
        | None ->
          return (`Eof,mk_content (`Multipart (List.rev contents)))
        | Some line -> 
          if Boundary.is_open boundary line then (
            let rec consume_multipart contents =
              Email.parse ~content_type ~content_subtype ?boundary 
                reader >>= fun (res,email) ->
              let contents = email :: contents in
              match res with
              | `Ok -> consume_multipart contents
              | _ ->
                match email.content.content with
                | `Multipart _ -> read contents
                | _ ->
                  return (res,mk_content (`Multipart (List.rev contents)))
            in
            consume_multipart contents
          ) else if Boundary.is_close boundary line then (
            return (`OkMultipart, mk_content (`Multipart (List.rev contents)))
          ) else
            read contents
      in
      read []
    ) else if content_type = `Message then (
      Email.parse ~content_type ~content_subtype ?boundary 
        reader >>= fun (res,email) ->
      return (res, mk_content (`Message email))
    ) else (
      let rec read () =
        MemReader.read_line_opt reader >>= function
        | None -> return (`Eof, mk_content `Data)
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
              adjust_for_boundary reader (String.length line) >>= fun adj ->
              return (`Ok, mk_content ~adj `Data)
            ) else if Boundary.is_close boundary line then (
              adjust_for_boundary reader (String.length line) >>= fun adj ->
              return (`OkMultipart, mk_content ~adj `Data)
            ) else
              read ()
      in
      read ()
    )
end
and 
Email:
sig
  type t = lightmail
  val parse : ?content_type:ctype -> ?content_subtype:stype ->
    ?boundary:boundary -> MemReader.t -> ([`Ok|`Eof|`OkMultipart] * t) Lwt.t
end =
struct
  type t = lightmail
  let parse ?(content_type=`Text) ?(content_subtype=`Plain) ?boundary reader =
    let position = {offset = MemReader.position reader; size = 0} in
    Headers.parse ~content_type ~content_subtype reader >>= fun (_,headers) ->
    let boundary =
      match headers.boundary with
      | None -> boundary
      | Some boundary -> Some boundary
    in
    Content.parse ~content_type ~content_subtype ?boundary 
      reader >>= fun (res,content) ->
    let position = {position with size = get_size reader position.offset} in
    return (res,{position;headers;content})
end

module Postmark :
sig
  type t = entity
  val parse : MemReader.t -> t Lwt.t
end = struct
  type t = entity
  let re_postmark = Re_posix.compile_pat ~opts:[`ICase] 
    ("^(From [^ \r\n]+ (mon|tue|wed|thu|fri|sat|sun) " ^
    "(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec) ([^\r\n]+))$")
  let parse reader =
    MemReader.read_line reader >>= fun line ->
    if Re.execp re_postmark line then
      return {offset = 0; size = String.length line}
    else (
      MemReader.set_position reader 0 >>
      return {offset = 0; size = 0}
    )
end

(* assume single, complete message *)
module Message :
sig
  type t = lightmessage
  val parse : string -> t Lwt.t
end =
struct
  type t = lightmessage
  let parse message =
   MemReader.of_string message >>= fun reader ->
   Postmark.parse reader >>= fun postmark ->
   Email.parse reader >>= fun (res,email) ->
   return {position={offset=0;size=String.length message}; postmark; email}
end
