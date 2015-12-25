open Lwt
open Sexplib.Std

exception MultipartNoBoundary

module MapStr = Map.Make(String)

type ctype = 
  [`Audio|`Video|`Image|`Application|`Text|`Multipart|`Message|`Other of string]
  with sexp
type stype = 
  [`Plain|`Rfc822|`Digest|`Alternative|`Parallel|`Mixed|`Other of string] with sexp
type entity = {offset:int; size:int} with sexp
type boundary = {bopen:string;bclose:string} with sexp
type lightheaders = {position:entity;content_type:ctype; content_subtype:stype; 
  boundary:boundary option;} with sexp
type lightcontent = {position:entity;content:
  [`Data|`Message of lightmail |`Multipart of lightmail list]}
and lightmail = {position:entity;headers:lightheaders; body: lightcontent} with sexp
type lightmessage = {position:entity;postmark:entity;email:lightmail} with sexp

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

  let to_string t = t.message
end

(* get the size for the given position to the current position *)
let get_size reader position =
  (MemReader.position reader) - position

(* create entity instance *)
let mk_position ?(adj=0) reader offset =
  let size = (get_size reader offset) - adj in
  {offset;size}

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

(* email headers *)
module Headers :
sig
  type t = lightheaders
  (* parse headers *)
  val parse : ?content_type:ctype -> ?content_subtype:stype -> MemReader.t -> 
    ([`Ok|`Eof] * t) Lwt.t
  (* get headers as the string *)
  val to_string : t -> string -> string
  (* get headers as the list *)
  val to_list : t -> string -> (string * string ) list Lwt.t
  (* get headers as the map *)
  val to_map : t -> string -> string Map.Make(String).t Lwt.t
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
    let position = {offset; size = 0} in
    let header =
      {content_type;content_subtype;boundary=None;position;} in
    read false false header

  let to_string (t:lightheaders) message =
    String.sub message t.position.offset t.position.size

  let get_headers t message cb init =
    let re = Re_posix.compile_pat "^([^\t :]+):[ ]*(.*)$" in
    let bytes = Lwt_bytes.of_string (to_string t message) in
    let ic = Lwt_io.of_bytes ~mode:Lwt_io.Input bytes in
    let rec read name value acc =
      Lwt_io.read_line_opt ic >>= function
      | None -> return (name,value,acc)
      | Some line ->
        try
          let subs = Re.exec re line in
          let acc = 
            (* call with last consume header/value. new header means all 
             * (if any) values with FWS are consumed
             *)
            if name != "" then
              cb acc name value
            else
              acc
          in
          let name = Re.get subs 1 in
          let value = Re.get subs 2 in
          read name value acc
        with Not_found -> (* must FWS *)
          let value = 
            if name <> "" then 
              String.concat " " [value; String.trim line] 
            else 
              value 
          in
          read name value acc
    in
    read "" "" init >>= fun (name,value,acc) ->
    Lwt_io.close ic >>= fun () ->
    let acc =
      if name <> "" then
        cb acc name value
      else
        acc
    in
    return acc

  let to_list t message =
    get_headers t message (fun acc name value ->
      (name,value) :: acc) [] >>= fun l ->
    return (List.rev l)

  let to_map t message =
    get_headers t message (fun acc name value ->
      MapStr.add name value acc
    ) MapStr.empty
end

(* define body content part *)
module rec Content:
sig
  type t = lightcontent
  (* parse content *)
  val parse : ?content_type:ctype -> ?content_subtype:stype ->
    ?boundary:boundary -> MemReader.t -> ([`Ok|`Eof|`OkMultipart] * t) Lwt.t
  (* get content string *)
  val to_string: t -> string -> string
end =
struct
  type t = lightcontent

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
                match email.body.content with
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

  let to_string (t:lightcontent) message =
    String.sub message t.position.offset t.position.size
end
and 
(* define email *)
Email:
sig
  type t = lightmail
  (* parse email *)
  val parse : ?content_type:ctype -> ?content_subtype:stype ->
    ?boundary:boundary -> MemReader.t -> ([`Ok|`Eof|`OkMultipart] * t) Lwt.t
  (* get email string headers+body *)
  val to_string : t -> string -> string
  (* get body *)
  val body : t -> lightcontent
  (* get headers *)
  val headers : t -> lightheaders
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
    let position = {offset; size} in
    return (res,{position;headers;body=content})

  let to_string (t:lightmail) message =
    String.sub message t.position.offset t.position.size

  let body (t:lightmail) =
    t.body

  let headers (t:lightmail) =
    t.headers
end

(* define postmark *)
module Postmark :
sig
  type t = entity
  (* parse postmark *)
  val parse : MemReader.t -> t Lwt.t
  (* get postmark as string *)
  val to_string : t -> string -> string
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

  let to_string (t:entity) message =
    String.sub message t.offset t.size
end

(* define message postmark+email *)
module Message :
sig
  type t = lightmessage
  (* parse message *)
  val parse : string -> t Lwt.t
  (* get message as string *)
  val to_string : t -> string -> string
end =
struct
  type t = lightmessage

  let parse message =
   MemReader.of_string message >>= fun reader ->
   Postmark.parse reader >>= fun postmark ->
   Email.parse reader >>= fun (res,email) ->
     return {position={offset=0;size=String.length message}; postmark; email}

  let to_string (t:lightmessage) message =
    String.sub message t.position.offset t.position.size
end
