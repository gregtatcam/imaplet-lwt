open Sexplib.Conv
open Core_replace

module Params = struct
  module Assoc = Field_name.Assoc_concrete (String);;
  include Assoc.Make;;

  let to_string_monoid t =
    let field_to_string_monoid (name, body) =
      let body = Rfc.RFC2045.Token.is_valid_or_quote body in
      String_monoid.concat_string [name; "="; body]
    in
    String_monoid.concat
      ~sep:(String_monoid.of_string "; ")
        (map_to_list t ~f:field_to_string_monoid)
  ;;
end

type t = {
  mime_type : string;
  mime_subtype : string;
  params : Params.t;
} with fields, sexp

let __UNUSED_VALUE__field_name = "content-type";;

let is ?a ?b t =
  Option_.value_map a ~default:true
    ~f:(Rfc.RFC2045.Token.equal t.mime_type)
  &&
  Option_.value_map b ~default:true
    ~f:(Rfc.RFC2045.Token.equal t.mime_subtype)
;;

(* Some convenience functions for working with mime types *)
let is_multipart t = is ~a:"multipart" t;;
let is_message_rfc2822 t = is ~a:"message" ~b:"rfc2822" t;;
let is_digest t = is ~a:"multipart" ~b:"digest" t;;

let is_composite t = is_multipart t || is_message_rfc2822 t
let is_simple t = not (is_composite t)
let is_text t = is ~a:"text" t

let mode t =
  if is_multipart t || is_text t || is_message_rfc2822 t then
    `Text
  else
    (* Unrecognized types are treated as application/octet-stream,
      that is, binary *)
    `Binary
;;

let multipart_boundary t =
  if is_multipart t then
    Option_.map ~f:Boundary.create (Params.last t.params "boundary")
  else
    None
;;

let of_grammar (mime_type, mime_subtype, params) =
  {
    mime_type = mime_type;
    mime_subtype = mime_subtype;
    params = Field_name.Assoc.of_rev_list params;
  }
;;

let of_string x =
  of_grammar (Grammar.content_type
  Lexer.content_type (Lexing.from_string x))
;;

let to_string_monoid t =
  String_monoid.plus
    (String_monoid.concat_string [t.mime_type; "/"; t.mime_subtype; " "])
    (if Params.is_empty t.params then
      String_monoid.empty
    else
      String_monoid.plus
      (String_monoid.of_string "; ")
      (Params.to_string_monoid t.params))
;;

let to_string t = String_monoid.to_string (to_string_monoid t);;


