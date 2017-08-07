(** Immutable sequences of bytes which can be windowed efficiently. *)
open Sexplib.Conv
open Core_replace

type t =
  {
    text    : bool;
    content : Bigstring_.t
  } [@@deriving sexp];;


let fix_eol_in_place bstr =
  let len =
    let len = Bigstring_.length bstr in
    Bigstring_extended.foldi bstr
      ~init:0
      ~f:(fun pos_src pos_dst c ->
        let next = pos_src + 1 in
        if c <> '\r' || next >= len || bstr.{next} <> '\n' then
        begin
          bstr.{pos_dst} <- c;
          pos_dst + 1
        end
        else
          pos_dst)
  in
  Bigstring_.sub_shared ~len bstr
;;

let create ?(mode=`Text) ?(fix_win_eol=false) bstr =
  match mode with
  | `Text           ->
    {
      text = true;
      content =
        if fix_win_eol then
          fix_eol_in_place bstr
        else
          bstr
    }
  | `Binary         -> { text = false; content = bstr }
;;

let contents t = t.content

let mode_set t mode =
  { t with text =
    match mode with
    | `Text -> true
    | `Binary -> false
  }
;;

let mode t = if t.text then `Text else `Binary

let is_text t = t.text
let is_binary t = not (is_text t)

let empty = create Bigstring_extended.empty

let of_string str = create (Bigstring_.of_string str)
let to_string t = Bigstring_.to_string (contents t)

(** Bigstring_.sub creates copies of the Bigstring *)
let of_bigstring bstr = create (Bigstring_.subo bstr)
let to_bigstring t = Bigstring_.subo (contents t)


let of_string_monoid mon = of_bigstring (String_monoid.to_bigstring mon)
let to_string_monoid t = String_monoid.of_bigstring (contents t)

let length t  = Bigstring_.length t.content

let sub ?pos ?len t =
  let pos, len = match pos, len with
  | None, None         -> 0, length t
  | None, Some len     -> 0, len
  | Some pos, None     -> pos, ((length t) - pos)
  | Some pos, Some len -> pos, len
  in
  { t with content = Bigstring_.sub_shared ~pos ~len t.content }
;;

let to_lexbuf t = Bigstring_extended.to_lexbuf t.content;;


(********)

module Base64 = struct

  let decode ?(mode=`Binary) t =
    (* Depending on encoding:
      If encoding is text, CRLF sequences are turned into LF.
      If encoding is binary, CRLF sequences are considered regular byte
        sequences.
    *)
    let bigbuffer, _ =
      Lexer.decode_base64
        ~is_text:(mode = `Text)
      (length t) (to_lexbuf t)
    in
    create ~mode (Bigstring_extended.of_bigbuffer_volatile bigbuffer)
  ;;

  let encode t =
    (* Depending on encoding:
      If t is text, all LF line endings are written as CRLF.
      If t is binary, do nothing.
    *)
    let bigbuffer =
      Lexer.encode_base64 ~is_text:(is_text t) (length t) (to_lexbuf t)
    in
    create ~mode:`Text
      (Bigstring_extended.of_bigbuffer_volatile bigbuffer)
  ;;

end

module Quoted_printable = struct

  let decode ?mode t =
    (* The RFC2045 says that newlines can be converted to the platforms native
      format, so that's what we'll do.
      It's the same for both binary data and text data.
      If a CRLF sequence appears in the decoded data, that's because it
      was encoded as =0D=0A, which means the characters shouldn't be interpreted
      as EOL.
    *)
    let bigbuffer, _ = Lexer.decode_quoted_printable (length t) (to_lexbuf t) in
    create ?mode (Bigstring_extended.of_bigbuffer_volatile bigbuffer)
  ;;

  let encode t =
    let bigbuffer =
      Lexer.encode_quoted_printable (length t) ~is_text:(is_text t) (to_lexbuf t)
    in
    (* Even if the original string was binary content, it turns into text
      when quoted-printable-encoded. *)
    create ~mode:`Text (Bigstring_extended.of_bigbuffer_volatile bigbuffer)
  ;;

end

module Identity = struct
  let encode t = t
  let decode ?mode t =
    match mode with
    | Some mode -> mode_set t mode
    | None      -> t
end

