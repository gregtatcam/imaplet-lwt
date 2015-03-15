module type S = sig
  type t

  val to_lexbuf : t -> Lexing.lexbuf
end
