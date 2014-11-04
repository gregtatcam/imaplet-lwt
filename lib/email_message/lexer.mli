open Core_replace

val message : Lexer_state.t -> Lexing.lexbuf -> Grammar.token 
val content_type : Lexing.lexbuf -> Grammar.token 
val find_boundary : string -> Lexing.lexbuf ->
  [ `Open_boundary_first of int |
    `Open_boundary of (int * int) |
    `Close_boundary of (int * int) |
    `Eof ]

val is_rfc2045_token : Lexing.lexbuf -> bool

val field_unstructured_fold : Bigbuffer_.t -> Lexing.lexbuf -> unit
val field_unstructured_unfold : Bigbuffer_.t -> Lexing.lexbuf -> unit

val field_token: Lexing.lexbuf -> (string option, string) Result_.t

val decode_base64 : int -> is_text:bool -> Lexing.lexbuf -> 
  (Bigbuffer_.t * [`Ok | `Unexpected_characters | `Wrong_padding ])
;;

val encode_base64 : int -> is_text:bool -> Lexing.lexbuf -> Bigbuffer_.t

val decode_quoted_printable : int -> Lexing.lexbuf -> 
  (Bigbuffer_.t * [`Ok | `Unexpected_characters ])
;;

val encode_quoted_printable : int -> is_text:bool -> Lexing.lexbuf -> Bigbuffer_.t
