module Debug_in_this_directory = Debug
module Debug = Debug_in_this_directory
open Sexplib.Conv
open Core_replace

type 'a sexp_option = 'a option

module rec Multipart : sig
  type t = {
    boundary : Boundary.t option;

    prologue : Octet_stream.t option;
    epilogue : Octet_stream.t option;

    parts    : Message.t list;
  } with sexp

  val of_octet_stream : media_type:Media_type.t ->
    Octet_stream.t -> t option

  val remove_raw_content : t -> t

  include String_monoidable.S with type t := t
end = struct
  type t = {
    boundary : Boundary.t sexp_option;

    prologue : Octet_stream.t sexp_option;
    epilogue : Octet_stream.t sexp_option;

    parts    : Message.t list;
  } with sexp

  let of_octet_stream ~media_type octet_stream =
    let open Option_.Monad_infix in
    Media_type.multipart_boundary media_type >>| fun boundary ->
    let prologue, parts, epilogue =
      Boundary.split_octet_stream ~octet_stream boundary
    in
    Debug.run_debug (fun () -> Printf.eprintf "Boundary: %s; Part count: %d\n"
      (Boundary.to_string boundary)
      (List.length parts));
    let parts =
      List.map (Message.of_octet_stream ~parent:(Some media_type)) parts;
    in
    {
      boundary = Some boundary;
      prologue = prologue;
      epilogue = epilogue;
      parts = parts
    }
  ;;

  let to_string_monoid t =
    let parts = List.map Message.to_string_monoid t.parts in

    let boundary = Boundary.generate ?suggest:t.boundary () in

    (* Different types of boundaries that may appear in a message *)
    let boundary_open = Boundary.Open.to_string_monoid boundary in
    let boundary_open_first = Boundary.Open_first.to_string_monoid boundary in
    let boundary_close = Boundary.Close.to_string_monoid boundary in

    let prologue_and_first_boundary =
      match t.prologue with
      | None -> boundary_open_first
      | Some prologue ->
          String_monoid.plus
            (Octet_stream.to_string_monoid prologue)
            boundary_open
    in
    String_monoid.concat [
      prologue_and_first_boundary;
      String_monoid.concat ~sep:boundary_open parts;
      boundary_close;
      Option_.value_map
        ~default:String_monoid.empty
        ~f:Octet_stream.to_string_monoid
        t.epilogue
      ;
    ]
  ;;

  let remove_raw_content t =
    { t with parts = List.map Message.remove_raw_content t.parts }
  ;;
end
and Content : sig
  type t =
      Multipart of Multipart.t
    | Data of (Encoding.t * Octet_stream.t)
    | Message of Message.t
  with sexp
  ;;

  val empty : unit -> t
  val of_octet_stream : header:Header.t ->
    parent:(Media_type.t option) -> Octet_stream.t -> t

  val remove_raw_content : t -> t

  val simple : t ->
    [ `Message of Message.t |
      `Data of Octet_stream.t |
      `Multipart of (Message.t list) ]
  ;;


  include String_monoidable.S with type t := t
end
= struct
  (* Message and multipart hold no encoding, as they must be encoded using
    7bit encoding with US-ASCII character set *)
  type t =
      Multipart of Multipart.t
    | Data of (Encoding.t * Octet_stream.t)
    | Message of Message.t
  with sexp
  ;;

  let empty () = Data (Header.Content_transfer_encoding.default, Octet_stream.empty)

  let of_octet_stream ~header ~parent octet_stream =
    let media_type =
      Option_.value (Header.Content_type.last header)
        ~default:(Header.Content_type.default ~parent)
    in
    let encoding =
      Option_.value (Header.Content_transfer_encoding.last header)
        ~default:(Header.Content_transfer_encoding.default)
    in
    let octet_stream =
      Encoding.decode_default encoding ~media_type octet_stream
    in
    if Media_type.is_message_rfc2822 media_type then
      Message
        (Message.of_octet_stream
          ~parent:(Some media_type)
          octet_stream)
    else
      match Multipart.of_octet_stream ~media_type octet_stream with
        | Some multipart -> Multipart multipart
        | None -> Data (encoding, octet_stream)
  ;;

  let to_string_monoid = function
    | Multipart multipart -> Multipart.to_string_monoid multipart
    | Message message -> Message.to_string_monoid message
    | Data (encoding, octet_stream) ->
      Octet_stream.to_string_monoid
        (Encoding.encode_default encoding octet_stream)
  ;;

  let simple = function
    | Message message -> `Message message
    | Multipart multipart -> `Multipart multipart.Multipart.parts
    | Data (_, octet_stream) -> `Data octet_stream
  ;;

  let remove_raw_content = function
    | Message message -> Message (Message.remove_raw_content message)
    | Multipart multipart -> Multipart (Multipart.remove_raw_content multipart)
    | (Data _) as content -> content
  ;;
end
and Message : sig
  type t with sexp;;

  val empty : unit -> t
  (* val create : ?header:Header.t -> ?content:Content.t -> unit -> t *)

  include String_monoidable.S with type t := t
  include Stringable_.S with type t := t
  include Bigstringable.S with type t := t

  val of_octet_stream : parent:(Media_type.t option) ->
    Octet_stream.t -> Message.t

  val header : t -> Header.t
  val content : t ->
    [ `Message of t | `Data of Octet_stream.t | `Multipart of (t list) ]
  ;;

  val set_header : t -> Header.t -> t

  val remove_raw_content : t -> t
  val raw_content : t -> Octet_stream.t option

end = struct

  type t = {
    header      : Header.t;
    raw_content : Octet_stream.t option;
    content     : Content.t Lazys.Lazy_m.t;
  }
  ;;

  let create ?(header=Header.empty) ?content () =
    {
      header = header;
      raw_content = None;
      content = Lazys.Lazy_m.of_val (Option_.value content ~default:(Content.empty ()));
    }
  ;;

  let empty () = create ()

  let of_grammar ~octet_stream ~parent (`Message (header, content_offset)) =
    let header = Header.of_rev_list header in
    (* Consider the body as empty if the message was truncated *)
    let octet_stream =
      match content_offset with
      | `Content_offset pos -> Octet_stream.sub ~pos octet_stream
      | `Truncated ->
        Debug.run_debug (fun () -> Printf.eprintf "Warning: Message truncated\n%!");
        Octet_stream.empty
    in
    let
      content = Lazys.Lazy_m.of_fun (fun () ->
        Content.of_octet_stream ~header ~parent octet_stream)
    in
    {
      header = header;
      raw_content = Some octet_stream;
      content = content;
    }
  ;;

  (*
    The default type of a message depends on the type of its parent,
    so we need to pass it around.
  *)
  let of_octet_stream ~parent octet_stream =
    let lexbuf = Octet_stream.to_lexbuf octet_stream in
    of_grammar ~octet_stream ~parent
      (Grammar.message (Lexer.message (Lexer_state.create ())) lexbuf)
  ;;

  let of_string str = of_octet_stream ~parent:None
    (Octet_stream.of_string str);;
  let of_bigstring bstr = of_octet_stream ~parent:None
    (Octet_stream.of_bigstring bstr)
  ;;

  let to_string_monoid t =
    let content_string_monoid =
      match t.raw_content with
      | Some octet_stream -> Octet_stream.to_string_monoid octet_stream
      | None -> Content.to_string_monoid (Lazys.Lazy_m.force t.content)
    in
    String_monoid.concat ~sep:String_monoid.nl [
      Header.to_string_monoid t.header;
      content_string_monoid
    ]
  ;;

  let to_string t = String_monoid.to_string (to_string_monoid t);;
  let to_bigstring t = String_monoid.to_bigstring (to_string_monoid t);;

  let header t = t.header;;
  let content t = Content.simple (Lazys.Lazy_m.force t.content);;
  let set_header t header = { t with header = header };;

  let raw_content t = t.raw_content
  let remove_raw_content t =
    { t with
      raw_content = None;
      content = (*Lazy_m.map t.content ~f:Content.remove_raw_content*)
        Lazys.Lazy_m.of_val (Content.remove_raw_content (Lazys.Lazy_m.force t.content))
    }
  ;;

  (* Auxiliary types for sexp *)
  module Sexp_aux = struct
    type t = {
      header : Header.t;
      content : Content.t;
    } with sexp
  end

  let t'_of_t t  =
    { Sexp_aux.
      header = t.header;
      content = Lazys.Lazy_m.force t.content;
    }
  ;;

  let t_of_t' t' = create
    ~header:t'.Sexp_aux.header
    ~content:t'.Sexp_aux.content
    ()
  ;;

  let sexp_of_t = Fn_.compose Sexp_aux.sexp_of_t t'_of_t
  let t_of_sexp = Fn_.compose t_of_t' Sexp_aux.t_of_sexp

end

include Message;;

let of_octet_stream t = of_octet_stream ~parent:None t;;

