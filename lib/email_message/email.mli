type t

val empty : unit -> t

val of_octet_stream : Octet_stream.t -> t

val header  : t -> Header.t
val set_header : t -> Header.t -> t

(** 
  Extracts the contents of a message in a simple form. 
  
  IF a message was created with 'of_octet_stream', and it's the first
  time the function it's called on the message, the cost
  depends on the encoding of the content and the main media type.

  N = Size of the message
  H = Size of the headers of the sub-message(s)
  
  Format: time complexity, memory complexity

            | 7bit, 8bit, binary | Base64, Quoted_printable
  -------------------------------------------------------------
  message   |    O(N), O(H)      | O(N), O(N)
  multipart |    O(N), O(H)      | O(N), O(N)
  *         |    O(1), O(1)      | O(N), O(N)

  Where * is any other main media type: text, image, application... 

  Encoding and type can be obtained from the headers, using the modules
  Header.Content_type and Header.Content_transfer_encoding, and the
  corresponding default values.

  ELSE, the cost is the same as that of forcing a lazy value.

  
*)
val content : t ->
  [ `Message of t | 
    `Data of Octet_stream.t | 
    `Multipart of (t list) 
  ]

(** Returns the raw body contents used to create the message, if any. *)
val raw_content : t -> Octet_stream.t option

(** Removes the raw contents from the structure. Useful for debugging
  purposes*)
val remove_raw_content : t -> t

(** String-builder-like module. Small-to-no memory overhead
  when unparsed. *) 
include String_monoidable.S with type t := t;;

include Stringable_.S with type t := t;;
include Bigstringable.S with type t := t;;
include Sexpable_.S with type t := t;;


