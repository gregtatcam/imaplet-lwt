open Sexplib.Conv
open Core_replace

module RFC2822 = struct
  let unfold str =
    let lexbuf = Lexing.from_string str in
    let bigbuffer = Bigbuffer_.create (String.length str) in
    Lexer.field_unstructured_unfold bigbuffer lexbuf;
    Bigbuffer_.contents bigbuffer
  ;;

  let fold str =
    let lexbuf = Lexing.from_string str in
    let bigbuffer = Bigbuffer_.create (String.length str) in
    Lexer.field_unstructured_fold bigbuffer lexbuf;
    Bigbuffer_.contents bigbuffer
  ;;

end

module RFC2045 = struct
  module Token = struct
    type t = string [@@deriving sexp]
    include (Mimestring.Case_insensitive : Mimestring.S with type t := t)

    let is_valid str = Lexer.is_rfc2045_token (Lexing.from_string str)
    let is_valid_or_quote str =
      if is_valid str then
        str
      else
        Mimestring.quote str

  end
end

