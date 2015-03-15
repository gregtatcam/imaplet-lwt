{
open Grammar
open Core_replace
module LS = Lexer_state

(* Can't open Core.Std, as the OCamlLex generated code requires Array.create
 * to be of type (int -> 'a -> 'a t), not (len:int -> 'a -> 'a t) 
module C = Core.Std*)

let force_token token _lexbuf = token

(*let unescape_staged = Core_kernel.Std.String.Escaping.unescape * ~escape_char:'\\';;*)
let unescape = Str.global_replace (Str.regexp "\\") "";;

module Decode_buffer = struct
  type text =
    {
      mutable hold_cr : bool;
      buffer  : Bigbuffer_.t;
    }
  ;;

  type t = Binary of Bigbuffer_.t | Text of text

  let create_text len =
    Text {
      hold_cr = false;
      buffer = Bigbuffer_.create len
    }
  ;;

  let create_binary len = Binary (Bigbuffer_.create len);;

  let add_char t c = match t with
  | Binary buffer -> Bigbuffer_.add_char buffer c
  | Text text ->
    match text.hold_cr, c with
    | false , '\r'        -> text.hold_cr <- true
    | false , c           -> Bigbuffer_.add_char text.buffer c
    | true  , ('\r' as c) -> Bigbuffer_.add_char text.buffer c
    | true  , ('\n' as c) ->
        text.hold_cr <- false;
        Bigbuffer_.add_char text.buffer c
    | true  , c ->
      text.hold_cr <- false;
      Bigbuffer_.add_char text.buffer '\r';
      Bigbuffer_.add_char text.buffer c
  ;;

  let to_bigbuffer = function
    | Binary buffer -> buffer
    | Text text ->
      if text.hold_cr then Bigbuffer_.add_char text.buffer '\r';
      text.buffer
  ;;
end

module Quantum
= struct

  (* Base 64 functions *)
  let base64to =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  ;;

  let base64from =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\062\255\255\255\063\
    \052\053\054\055\056\057\058\059\060\061\255\255\255\255\255\255\
    \255\000\001\002\003\004\005\006\007\008\009\010\011\012\013\014\
    \015\016\017\018\019\020\021\022\023\024\025\255\255\255\255\255\
    \255\026\027\028\029\030\031\032\033\034\035\036\037\038\039\040\
    \041\042\043\044\045\046\047\048\049\050\051\255\255\255\255\255"
  ;;

  (* Assuming ints are at least 31 bits long, so they have room
     for 3 bytes + 1 sign bit
   *)
  let __UNUSED_VALUE__empty = (0,0);;

  let add quantum len c ~buffer =
    let code = Char.code (base64from.[Char.code c]) in
    assert (code < 64);
    match len with
    | 0 -> (code, 1)
    | 1 -> ((quantum lsl 6) lor code, 2)
    | 2 -> ((quantum lsl 6) lor code, 3)
    | 3 ->
      let a = (quantum lsr 10) land 255 in
      let b = (quantum lsr 2) land 255 in
      let c = ((quantum lsl 6) lor code) land 255 in
      Decode_buffer.add_char buffer (Char.chr a);
      Decode_buffer.add_char buffer (Char.chr b);
      Decode_buffer.add_char buffer (Char.chr c);
      (0, 0)
    | _ -> assert false
  ;;

  let flush quantum len ~buffer =
    match len with
    | 0 -> ()
    | 1 -> ()
    | 2 ->
      let quantum = quantum lsr 4 in
      let a = quantum land 255 in
      Decode_buffer.add_char buffer (Char.chr a)
    | 3 ->
      let quantum = (quantum lsr 2) in
      let a = (quantum lsr 8) land 255 in
      let b = quantum land 255 in
      Decode_buffer.add_char buffer (Char.chr a);
      Decode_buffer.add_char buffer (Char.chr b)
    | _ -> assert false
  ;;

  let padding ~len ~padding =
    match len, padding with
    | 0, 0 -> true
    | 1, _ -> false
    | 2, 2 -> true
    | 3, 1 -> true
    | _    -> false
  ;;

end

module Encode_buffer = struct
  type t =
    {
      char_buffer     : string;

      (* Invariant: 0 < char_buffer_pos < 3 *)
      mutable char_buffer_pos : int;
      buffer          : Bigbuffer_.t;
      wrap            : int option;
      mutable linepos : int;
      is_text         : bool;
    }
  ;;

  let create ~is_text len =
    {
      char_buffer = "  ";
      char_buffer_pos = 0;
      buffer = Bigbuffer_.create len;
      wrap = Some 76;
      linepos = 0;
      is_text = is_text;
    }
  ;;

  (* Wrap_line is idempotent *)
  let wrap_line t =
    t.linepos <-
      begin
      match t.wrap with
      | None -> -1
      | Some wrap ->
        if t.linepos + 4 > wrap then
        begin
          Bigbuffer_.add_string t.buffer "\n";
          0
        end
        else
          t.linepos + 4
      end
  ;;

  let add_char' t c =
    if t.char_buffer_pos >= 2 then
    begin
      assert (t.char_buffer_pos = 2);
      let a, b, c = t.char_buffer.[0], t.char_buffer.[1], c in
      wrap_line t;
      t.char_buffer_pos <- 0;
      let quantum =
        ((Char.code a) lsl 16) +
        ((Char.code b) lsl 8)  +
        ((Char.code c))
      in
      let a =  quantum lsr 18 in
      let b = (quantum lsr 12) land 63 in
      let c = (quantum lsr 6) land 63 in
      let d = quantum land 63 in
      Bigbuffer_.add_char t.buffer (Quantum.base64to.[a]);
      Bigbuffer_.add_char t.buffer (Quantum.base64to.[b]);
      Bigbuffer_.add_char t.buffer (Quantum.base64to.[c]);
      Bigbuffer_.add_char t.buffer (Quantum.base64to.[d])
    end
    else
    begin
      t.char_buffer.[t.char_buffer_pos] <- c;
      t.char_buffer_pos <- t.char_buffer_pos + 1
    end
  ;;

  let add_char t c =
    match t.is_text, c with
    | true, '\n' ->
      add_char' t '\r';
      add_char' t '\n'
    | _, c      -> add_char' t c
  ;;

  (* Flush is idempotent *)
  let flush t =
    wrap_line t;
    match t.char_buffer_pos with
    | 2 ->
      let b, c = t.char_buffer.[0], t.char_buffer.[1] in
      t.char_buffer_pos <- 0;
      let quantum =
        ((Char.code b) lsl 10)  +
        ((Char.code c) lsl 2)
      in
      let b = (quantum lsr 12) land 63 in
      let c = (quantum lsr 6) land 63 in
      let d = quantum land 63 in
      Bigbuffer_.add_char t.buffer (Quantum.base64to.[b]);
      Bigbuffer_.add_char t.buffer (Quantum.base64to.[c]);
      Bigbuffer_.add_char t.buffer (Quantum.base64to.[d]);
      Bigbuffer_.add_char t.buffer '='
    | 1 ->
      let c = t.char_buffer.[0] in
      t.char_buffer_pos <- 0;
      let quantum =
        ((Char.code c) lsl 4)
      in
      let c = (quantum lsr 6) land 63 in
      let d = quantum land 63 in
      Bigbuffer_.add_char t.buffer (Quantum.base64to.[c]);
      Bigbuffer_.add_char t.buffer (Quantum.base64to.[d]);
      Bigbuffer_.add_char t.buffer '=';
      Bigbuffer_.add_char t.buffer '='
    | 0 ->
      ()
    | _ -> assert false
  ;;

  let to_bigbuffer t =
    flush t;
    t.buffer
  ;;

end


module Quoted_printable = struct

  (* Quoted printable functions *)
  let decode_hex a b =
    let decode_digit c = match c with
      | '0'..'9' -> (Char.code c) - (Char.code '0') + 0
      | 'A'..'F' -> (Char.code c) - (Char.code 'A') + 10
      | 'a'..'f' -> (Char.code c) - (Char.code 'a') + 10
      | _ -> invalid_arg "c"
    in
    Char.chr (((decode_digit a) * 16) + (decode_digit b))
  ;;

  let hex_to = "0123456789ABCDEF"

  module Buffer = struct
    type t =
      {
        max_len : int;
        text : Bigbuffer_.t;

        (** Invariant: The length of `word' is always less than `max_len' *)
        word : Bigbuffer_.t;

        (** Invariant: `pos' is always less than `max_len' *)
        mutable pos  : int;
      }
    ;;

    let create len =
      {
        text = Bigbuffer_.create len;
        word = Bigbuffer_.create 16;
        max_len = 76;
        pos = 0;
      }
    ;;

    let add_break t =
      if t.pos > 0 then
      begin
        Bigbuffer_.add_string t.text "=\n";
        t.pos <- 0
      end
    ;;

    (** Adds the current word to the buffer, wrapping it to the
      next line if necessary *)
    let commit_word t =
      if t.pos + Bigbuffer_.length t.word >= t.max_len then add_break t;

      Bigbuffer_.add_buffer t.text t.word;
      t.pos <- t.pos + Bigbuffer_.length t.word;
      Bigbuffer_.clear t.word
      ;
    ;;

    let wrap_bigword t len_next =
      if Bigbuffer_.length t.word >= t.max_len - len_next then
      begin
        add_break t;
        commit_word t;
        add_break t;
      end
    ;;

    let add_char t c =
      wrap_bigword t 1;
      Bigbuffer_.add_char t.word c;
    ;;


    let add_quoted_char t c =
      wrap_bigword t 3;
      let code = Char.code c in
      let high = (code lsr 4) land (16 - 1) in
      let low = code land (16 - 1) in
      Bigbuffer_.add_char t.word '=';
      Bigbuffer_.add_char t.word hex_to.[high];
      Bigbuffer_.add_char t.word hex_to.[low]
    ;;

    (* When calling this function, one must not immediately
      call add_new_line.
    *)
    let add_wsp t c =
      add_char t c;
      commit_word t
    ;;

    let add_new_line t c =
      Option_.iter c ~f:(fun c -> add_quoted_char t c);
      commit_word t;
      Bigbuffer_.add_char t.text '\n';
      t.pos <- 0
    ;;

    let to_bigbuffer t =
      commit_word t;
      t.text
    ;;
  end

end



}
(* Rules from RFCs. Some may need to be copied to specific places to parse
 * their individual parts *)

(** RFC2234 - Core *)
let cr = "\013"
let lf = "\010"

(* XXX: Should be just cr lf *)
(* Deliberately chose to include bare CR and LF here, although the RFC suggests
 * them to be included as part of the text characters.
 * The rationale being that, when parsing e-mail from a text file, they will
 * probably mean CRLF.
 * The issue should not arise in conforming e-mails.
 *)
let crlf_conforming = cr lf
let crlf_non_conforming = crlf_conforming | lf
let crlf = crlf_non_conforming

let wsp = [' ' '\t']

let upper_alpha = [ 'A'-'Z' ]
let lower_alpha = [ 'a'-'z' ]
let alpha = upper_alpha | lower_alpha
let digit = ['0'-'9']

(** RFC2822 3.2.1 -- Primitive tokens *)

let no_ws_ctl = [ '\001'-'\008' '\011' '\012' '\014'-'\031' '\127']

(* See crlf *)
let text_non_conforming = [ '\001'-'\009' '\011' '\012' '\014'-'\127' ]

let specials = [ '(' ')' '<' '>' '[' ']' ':' ';' '@' '\\' ',' '.' '"' ]

(** RFC2822 3.2.2 -- Quoted characters  *)
let obs_qp = "\\" [ '\000' - '\127' ]
(* Allows for escaping of newlines *)
let crlf_qp_non_conforming = "\\" crlf

(** XXX: crlf-qp-non-conforming shouldn't be here. *)
let quoted_pair = "\\" text_non_conforming | crlf_qp_non_conforming | obs_qp

(** RFC2822 3.2.3 -- Folding whitespace and comments *)
(* Obs: If this matches a CRLF, there's forcibly wspafterwards *)
let obs_fws = wsp + (crlf wsp +) *
let fws = ((wsp * crlf) ? wsp + ) | obs_fws

let ctext = no_ws_ctl | [ '\033'-'\039' '\042'-'\091' '\093'-'\126' ]
(*
Comments can't be expressed with regular expressions, as they're nested:

let ccontent = ctext | quoted_pair | comment
let comment = "(" (fws ? ccontent) * fws ? ")"
let cfws = (fws ?comment) * ((fws ? comment) | fws)
*)

(** RFC2822 3.2.4 -- Atom *)
let atext = alpha | digit |
  [ '!' '#' '$' '%' '&' '\'' '*' '/' '=' '?' '^' '_' '`' '{' '|' '}' '~']
(*
let atom = cfws ? atext + cfws ?
let dot_atom_text = atext + ("." atext +)*
let dot_atom = cfws ? dot_atom_text cfws ?
*)

(** RFC2822 3.2.5 -- Quoted strings *)
let qtext = no_ws_ctl | [ '\033' '\035'-'\091' '\093'-'\126' ]
let qcontent = qtext | quoted_pair

let quoted_string_contents = (fws ? qcontent) * fws ?
(*
let quoted_string = cfws ? '"' quoted_string_contents '"' cfws ?
(** RFC2822 3.2.6 -- Miscellaneous tokens *)
let word = atom | quoted_string

let obs_phrase = word (word | "." | cfws) *
let phrase = word + | obs_phrase
*)

let obs_char = [^ '\n' '\r' ]
(* XXX: Non-conforming. Should include obs-utext *)
let obs_text = lf * cr * (obs_char lf * cr *)

(* Match lone CRs *)
(* This matching is not perfect, as in this case:
  - \r\r\r\r..\r\n
    It might not consider the last \r as part of a CRLF line ending.
    However, it fulfils the following properties:
    * On standards-compliant text, it will never match a CR that is part
      of a CRLF line-ending; standards compliant meaning that it doesn't
      have any lone CR pairs.
    * It works as expected on texts with lone CRs.
*)
let obs_cr_text = cr + [ ^ '\r' '\n']


let utext =
  no_ws_ctl |
  [ '\033' - '\126' ] |
  obs_char |
  obs_cr_text

let unstructured = (fws ? utext) * fws ?


(**********************************************************)

(* Header fields *)
let ftext = [ '\033'-'\057' '\059'-'\126' ]
let field_name = ftext +
let optional_field = field_name ":" unstructured crlf

(**********************************************************)

(* RFC 2045 5.1 -- Syntax of the Content-type header field *)
(* Removed space characters, control characters, those outsize US-ASCII and
 * tspecials *)
let token_char =
  ([ ^ '\n' '\r'  ' '
       '(' ')' '<' '>' '@' ',' ';' ':' '\\' '"' '/' '[' ']' '?' '='
       '\000' - '\031' '\127' ])

let token = token_char +

let token_char_strict = (token_char # [ '\128' - '\255' ])

let token_strict = token_char_strict +

(* RFC 2046 5.1.1 -- Common syntax *)
let bcharnospace = (digit | alpha |
  ['\'' '(' ')' '+' '_' ',' '-'  '.' '/' ':' '=' '?'])

let bchar = bcharnospace | ' '

let b = bchar ?

(* 1 + 64 + 5 = 70 is what the standard requires *)
let boundary_name = bcharnospace (b *)

(* Base 64 *)
let b64c = [ 'A'-'Z' 'a'-'z' '0'-'9' '+' '/' ]
let b64pad = ['=']

(* Characters to ignore *)
let b64wsp = wsp
let b64i = (_ # b64c) # b64pad

(* Quoted-printable *)
let hexdigit_non_strict = ['0'-'9' 'a'-'f' 'A'-'F']
let hexdigit_strict = hexdigit_non_strict # ['a'-'f']
let qp_wsp = [' ' '\t']

let qp_allowed = [ '\033'-'\060' '\062'-'\126' ] as c

(* MESSAGE PARSING RULES *)

(* This lexer incorporates explicit state to allow it to process t
   the different parts of the email. This is because the tokens
   we want to generate depend on the context we are in.

   Another posibility is to handle everything in the grammar. However,
   each of the terminal symbols (characters) being a different
   state incurs significant overhead (both runtime and developing
   time).

   Statefulness is distasteful, so we keep it to a mininum:
     - Small number of states and transitions.
     - State is only read and set in one place, in the
       dispatcher.
*)

rule
(* Dispatcher *)
message t = parse
  | ""
  {
    (*match (try (Some (Queue.pop t.LS.buf)) with _ -> None) with*)
    try
    let tok = Queue.pop (t.LS.buf) in force_token tok lexbuf
    with _ ->
      let result =
        match t.LS.state with
        (* Tokenization is vastly different depending on the context *)
        | `Header -> field lexbuf
        | `Content -> body_octet_stream lexbuf
        | `Expected_eof -> expected_eof lexbuf
      in
      begin
      LS.combine t result;
      message t lexbuf
      end
    }
and
(** Looks for a header name *)
field = parse
  | (field_name as name)
    (wsp * as _wsp)
     ":"
    (unstructured as body)
    crlf
    {
      (* This code is repeated to avoid cyclical dependencies. Effort has
       * been made to make it minimal.
       *)
      LS.return [FIELD(name, body)]
    }
  | crlf { LS.return ~new_state:`Content [HEADER_END] }
  | eof  { LS.return_eof }

and

(** This rule throws an error if there are any more characters in the file *)
body_octet_stream =
  parse
  | "" | eof
  {
    assert (Lexing.lexeme_start lexbuf = Lexing.lexeme_end lexbuf);
    let pos = Lexing.lexeme_start lexbuf in
    LS.return ~new_state:`Expected_eof [ OCTET_STREAM_OFFSET (pos); EOF ]
  }
and


(* Supporting functions *)
error =
  parse
    | _ as c { LS.return_error (Printf.sprintf "Unexpected character %c" c) }
    | eof    { LS.return_error "Unexpected EOF" }
and
expected_eof =
  parse
  | eof    { LS.return_eof }
  | ""     { error lexbuf }

(******************)
(* HEADER PARSING *)
(******************)
(* These lexers are independent from the former, and work the way traditional
lexers do, without a dispatcher or state handlers *)


(** Allows other lexers to skips comments.

   Example usage:
rule my_lexer = parse
  | '('      -> { my_lexer (comment 1 lexbuf) }
  | .. other patterns ..

*)
and
comment level = parse
  | '('            { comment (level + 1) lexbuf }
  | ')' fws *      {
    if level <= 1 then
    begin
      assert (level = 1); lexbuf
    end
    else
      comment (level - 1) lexbuf }
  | '\\' ? _ { comment level lexbuf }
and

(* Parses a Content-type field *)
content_type = parse
  | '('          { content_type (comment 1 lexbuf) }
  | fws          { content_type lexbuf }
  | '"' (quoted_string_contents as str) '"' { STRING (unescape str) }
  | '/'          { SLASH }
  | token as str { ATOM (str) }
  | '='          { EQUALS }
  | ';'          { SEMICOLON }
  | _ as c       { ERROR (LS.Error.unexpected_char c) }
  | eof          { EOF }

and
(* Parses a field with only one token. Ignore the following tokens. *)
field_token = parse
  | fws          { field_token lexbuf }
  | '('          { field_token (comment 1 lexbuf) }
  | token as str { Result_.Ok (Some str) }
  | '"' (quoted_string_contents as str) '"'
    { Result_.Ok (Some (unescape str)) }
  | _ as c       { Result_.Error (LS.Error.unexpected_char c) }
  | eof          { Result_.Ok None }

and
(* Unfolds an Unstructured field, as per RFC2822 *)
field_unstructured_unfold buffer = parse
  | crlf ((wsp as c) ?) {
      Bigbuffer_.add_char buffer (Option_.value c ~default:' ');
      field_unstructured_unfold buffer lexbuf
  }
  | wsp as c    {
      if Bigbuffer_.length buffer > 0 then Bigbuffer_.add_char buffer c;
      field_unstructured_unfold buffer lexbuf
  }
  | _ as c          {
    Bigbuffer_.add_char buffer c;
    field_unstructured_unfold buffer lexbuf
  }
  | eof             { () }

and
(* Folds an Unstructured field, as per RFC2822 *)
field_unstructured_fold buffer = parse
  | (crlf (wsp as c)) {
      Bigbuffer_.add_char buffer '\n';
      Bigbuffer_.add_char buffer c;
      field_unstructured_fold buffer lexbuf
  }
  | crlf {
      Bigbuffer_.add_char buffer '\n';
      Bigbuffer_.add_char buffer ' ';
      field_unstructured_fold buffer lexbuf
  }
  | wsp as c {
      Bigbuffer_.add_char buffer c;
      field_unstructured_fold buffer lexbuf
  }
  | _ as c {
      if Bigbuffer_.length buffer = 0 then
        Bigbuffer_.add_char buffer ' ';

      Bigbuffer_.add_char buffer c;
      field_unstructured_fold buffer lexbuf
  }
  | eof             { () }

and
(* There are two very similar rules because the first boundary
 in the file might not begin with a CRLF sequence *)
find_boundary_first reference = parse
  | "--" {
    let start = Lexing.lexeme_start lexbuf in
    match_boundary_name start reference 0 true lexbuf
    }
  | ""   { find_boundary_inner reference lexbuf }
  | eof  { `Eof }

and
find_boundary_inner reference = parse
  | crlf "--" {
      let start = Lexing.lexeme_start lexbuf in
      match_boundary_name start reference 0 false lexbuf
    }
  | (crlf | _) { find_boundary_inner reference lexbuf }
  | eof { `Eof }

(*and
consume_rest_of_line cont = parse
  | crlf { cont lexbuf }
  | _    { consume_rest_of_line cont lexbuf }
  | eof { `Eof }*)

(* XXX: Important: No CRLF should be consumed by the rules below
  unless the CRLF does belong to a boundary *)
and
match_boundary_name start reference pos is_first = parse
  | bchar as c  {
      if c = reference.[pos] then
        let pos = pos + 1 in
        if pos = String.length reference then
          match_boundary_end start reference is_first lexbuf
        else
          match_boundary_name start reference pos is_first lexbuf
      else
        find_boundary_inner reference lexbuf
    }
  | "" { find_boundary_inner reference lexbuf }
  | eof { `Eof }

and
match_boundary_end start reference is_first = parse
  | wsp * crlf {
      if is_first then
        `Open_boundary_first (Lexing.lexeme_end lexbuf)
      else
        `Open_boundary (start, Lexing.lexeme_end lexbuf)
    }
  | "--" wsp * { `Close_boundary (start, Lexing.lexeme_end lexbuf) }
  | ""         { find_boundary_inner reference lexbuf }
  | eof        { `Eof }

(* VALIDATION *)
and
is_rfc2045_token = parse
  | token_strict eof { true }
  | token_strict { is_rfc2045_token lexbuf }
  | _            { false }
  | eof          { false }
and
(* Decodes Base64 content *)
decode_base64 buffer quantum quantum_len dirty padding = parse
  | b64c as c {
    if padding = 0 then
      let quantum, quantum_len = Quantum.add quantum quantum_len c ~buffer in
      decode_base64 buffer quantum quantum_len dirty padding lexbuf
    else
    begin
      Quantum.flush quantum quantum_len ~buffer;
      `Unexpected_characters
    end
  }
  | b64pad    {
      decode_base64 buffer quantum quantum_len dirty (padding + 1) lexbuf
    }
  | (crlf | b64wsp) *
    {
      decode_base64 buffer quantum quantum_len dirty padding lexbuf
    }
  | _
    {
      let dirty = `Unexpected_characters in
      decode_base64 buffer quantum quantum_len dirty padding lexbuf
    }
  | eof
    {
      Quantum.flush quantum quantum_len ~buffer;
      if Quantum.padding ~padding ~len:quantum_len then
        dirty
      else
        `Wrong_padding
    }
and
(* Encode Base64 content *)
encode_base64 buffer = parse
  | _ as c
    {
      Encode_buffer.add_char buffer c;
      encode_base64 buffer lexbuf
    }
  | eof
    {
      ()
    }

and

(*  Quoted-printable text *)
decode_quoted_printable buffer dirty = parse
  | wsp * crlf
      {
        Bigbuffer_.add_char buffer '\n';
        decode_quoted_printable buffer dirty lexbuf
      }
  | '=' crlf { decode_quoted_printable buffer dirty lexbuf }
  | '=' (hexdigit_strict as a) (hexdigit_strict as b)
      {
        Bigbuffer_.add_char buffer (Quoted_printable.decode_hex a b);
        decode_quoted_printable buffer dirty lexbuf
      }
  | '=' (hexdigit_non_strict as a) (hexdigit_non_strict as b)
      {
        Bigbuffer_.add_char buffer (Quoted_printable.decode_hex a b);
        decode_quoted_printable buffer `Unexpected_characters lexbuf
      }
  | (qp_allowed | wsp) as c
      {
        Bigbuffer_.add_char buffer c;
        decode_quoted_printable buffer dirty lexbuf;
      }
  | _ as c
      {
        (* This characters shoudn't appear in a quoted-printable body,
        buy the most robust way to handle them is probably copying
        them verbatim *)
        Bigbuffer_.add_char buffer c;
        decode_quoted_printable buffer `Unexpected_characters lexbuf
      }
  | eof { dirty }

and

(* Quoted-printable encoding with wrapping *)
encode_quoted_printable buffer = parse
  | (wsp as c) ? '\n'
    {
      Quoted_printable.Buffer.add_new_line buffer c;
      encode_quoted_printable buffer lexbuf
    }
  | wsp as c
    {
      Quoted_printable.Buffer.add_wsp buffer c;
      encode_quoted_printable buffer lexbuf
    }
  | qp_allowed as c
    {
      Quoted_printable.Buffer.add_char buffer c;
      encode_quoted_printable buffer lexbuf
    }
  | _ as c
    {
      Quoted_printable.Buffer.add_quoted_char buffer c;
      encode_quoted_printable buffer lexbuf
    }
  | eof
    {
      ()
    }
and

(* Quoted-printable encoding for binaries, with wrapping.
  There are two main differences:
    - Tabs are not encoded as themselves, but as =09. This
      helps protect them against MTAs which may replace tabs
      with some amount of spaces.
    - Newline characters CR and LF are encoded as =0D and
      =0A.
*)
encode_quoted_printable_binary buffer = parse
  | ' ' as c
    {
      Quoted_printable.Buffer.add_wsp buffer c;
      encode_quoted_printable_binary buffer lexbuf
    }
  | [ '\t' '\n' '\r' ] as c
    {
      Quoted_printable.Buffer.add_quoted_char buffer c;
      encode_quoted_printable_binary buffer lexbuf
    }
  | qp_allowed as c
    {
      Quoted_printable.Buffer.add_char buffer c;
      encode_quoted_printable_binary buffer lexbuf
    }
  | _ as c
    {
      Quoted_printable.Buffer.add_quoted_char buffer c;
      encode_quoted_printable_binary buffer lexbuf
    }
  | eof
    {
      ()
    }

{

let find_boundary reference lexbuf =
  if Lexing.(lexbuf.lex_curr_p.pos_cnum) = 0 then
    find_boundary_first reference lexbuf
  else
    find_boundary_inner reference lexbuf
;;

let decode_base64 len ~is_text lexbuf =
  let buffer =
    if is_text then
      Decode_buffer.create_text len
    else
      Decode_buffer.create_binary len
  in
  let warnings = decode_base64 buffer 0 0 `Ok 0 lexbuf in
  let bigbuffer = Decode_buffer.to_bigbuffer buffer in
  (bigbuffer, warnings)
;;

let encode_base64 len ~is_text lexbuf =
  let buffer = Encode_buffer.create ~is_text len in
  encode_base64 buffer lexbuf;
  let bigbuffer = Encode_buffer.to_bigbuffer buffer in
  bigbuffer
;;

let decode_quoted_printable len lexbuf =
  let length_estimate = len in
  let bigbuffer = Bigbuffer_.create length_estimate in
  let warnings = decode_quoted_printable bigbuffer `Ok lexbuf in
  (bigbuffer, warnings)
;;

let encode_quoted_printable len ~is_text lexbuf =
  let length_estimate = len in
  let buffer = Quoted_printable.Buffer.create length_estimate in

  if is_text then
    encode_quoted_printable buffer lexbuf
  else
    encode_quoted_printable_binary buffer lexbuf
  ;

  Quoted_printable.Buffer.to_bigbuffer buffer
;;

}

