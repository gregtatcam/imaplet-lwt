open Sexplib.Conv
open Core_replace
open Lazys

let with_file file ~f =
  let ic = Pervasives.open_in file in
  let a = f ic in
  Pervasives.close_in ic;
  a

let input_line ic =
  try
    Some (Pervasives.input_line ic)
  with _ -> None

module Regexp : sig
  include Email_regex.Accessors_intf

  (* All regexen and templates must be created inside this module *)
  val postmark : Email_regex.t;;
  val postmark' : Email_regex.t;;
  val escaped_line : Email_regex.t;;
  val blank : Email_regex.t;;

  val escape_line : Email_regex.Template.t;;
  (* val unix_line_terminator : Email_regex.Template.t;; *)

end
= struct
  open Email_regex.Creators;;

  (* Tokens *)
  let several str = "(" ^ str ^ "+)";;
  let maybe str = "(" ^ str ^ "?)";;
  let wsp = "[\t ]";;
  let wsp' = several wsp;;
  let no_wsp = "[^\t ]";;
  let no_wsp' = several no_wsp;;
  let __UNUSED_VALUE__any = ".*";;
  let alpha = "[A-Za-z]";;
  let alpha' = several alpha;;
  let digit = "[0-9]";;
  let digit' = several digit;;

(*  let cg name pattern = (Printf.sprintf "(?P<%s>%s)" name pattern);; *)
  let cg name pattern = pattern;;
  let of_list = String.concat "";;

  (* Regexen for parsing postmarks *)
  let datetime = of_list
  [
    cg "w" alpha'; wsp';
    cg "m" alpha'; wsp';
    cg "d" digit'; wsp';
    cg "h" digit'; ":"; cg "i" digit'; ":"; cg "s" digit'; wsp';
    cg "y" digit'
  ]
  ;;

  let postmark = create ("^From" ^ wsp ^ "+.*$");;
  let postmark' = create (of_list
    [ "^From"; wsp';
      cg "from" no_wsp'; wsp';
      cg "time" datetime; maybe wsp';
      "$"
    ]);;


  let escaped_line = create ("^>From" ^ wsp ^ ".*$");;
  let blank = create ("^" ^ wsp ^ "*$");;

  (* Templates *)
  let escape_line =
    Template.create
    ~regex:(create_m ("^From(" ^ wsp ^ ")"))
    ~template:">From\\1"

  include Email_regex.Accessors;;
end

module Postmark = struct
  type t = {
    from : string;
    time : float;
  } [@@deriving sexp]
  ;;

  let tdiff =
    let t = Unix.time() in
    let (tl,_) = Unix.mktime (Unix.localtime t) in
    let (tg,_) = Unix.mktime (Unix.gmtime t) in
    tl -. tg

  let of_tm tm =
    let (t,_) = Unix.mktime tm in
    t +. tdiff

  let day_of_week i =
    let dow = ["Sun";"Mon";"Tue";"Wed";"Thu";"Fri";"Sat"] in
    List.nth dow i

  let mon i = 
    let month =
      ["Jan";"Feb";"Mar";"Apr";"May";"Jun";"Jul";"Aug";"Sep";"Oct";"Nov";"Dec"] in
    List.nth month i

  let mon_to_int m =
    let month =
      ["jan";"feb";"mar";"apr";"may";"jun";"jul";"aug";"sep";"oct";"nov";"dec"] in
    let (i,_) = List.findi_exn month ~f:(fun i m_ -> (String.lowercase m) = m_) in
    i

  let to_string t =
    let open Unix in
    let tm = gmtime t.time in
    Printf.sprintf "From %s %s %s %02d %02d:%02d:%02d %4d"
      t.from
      (day_of_week tm.tm_wday)
      (mon tm.tm_mon)
      tm.tm_mday
      tm.tm_hour
      tm.tm_min
      tm.tm_sec
      (tm.tm_year+1900)
    (*
    let date, ofday = Time.to_date_ofday t.time Time.Zone.utc in
    let parts = Core.Ofday.to_parts ofday in

    Printf.sprintf "From %s %s %s %2d %02d:%02d:%02d %4d"
      t.from
      (String.capitalize
        (String.lowercase (Core_kernel.Day_of_week.to_string (Date.day_of_week date))))
      (Core_kernel.Month.to_string (Date.month date))
      (Date.day date)
      parts.Core.Span.Parts.hr
      parts.Core.Span.Parts.min
      parts.Core.Span.Parts.sec
      (Date.year date)
    *)
  ;;

  (*    (sprintf "%a %b %e %T %Y");; *)
  let parse str =
    let open Result_.Monad_infix in
    Result_.of_option Regexp.(apply postmark' str)
      ~error:"Maybe missing >"
    >>= (fun m ->
      try
        let module Match = Regexp.Match in
        let from = Match.by_name m "from" 2 in

        (*let _weekday = Core_kernel.Day_of_week.of_string (Match.by_name m "w") * in*)
        let y = Pervasives.int_of_string (Match.by_name m "y" 14) in
        (*let month = Core_kernel.Month.of_string (Match.by_name m "m") in*)
        let month = mon_to_int (Match.by_name m "m" 6) in
        let d = Pervasives.int_of_string (Match.by_name m "d" 8) in
        (*let date = Date.create_exn ~y ~m:month ~d in*)

        let hr = Pervasives.int_of_string (Match.by_name m "h" 10) in
        let min = Pervasives.int_of_string (Match.by_name m "i" 11) in
        let sec = Pervasives.int_of_string (Match.by_name m "s" 12) in
        (*let ofday = Core.Ofday.create ~hr ~min ~sec () in*)

        (*let time = Time.of_date_ofday Time.Zone.utc date ofday  in*)
        let tm = {Unix.tm_sec=sec;tm_min=min;tm_hour=hr;tm_mday=d;tm_mon=month-1;tm_year=y-1900;
            tm_wday=0;tm_yday=0;tm_isdst=false}
        in
        let time = of_tm tm in
        Result_.Ok { from = from; time = time }
      with
      e -> Result_.Error (Printexc.to_string e))
  |! Result_.map_error ~f:(Printf.sprintf "Unable to parse postmark %s: %s." str)
  ;;

  let of_string str = Result_.ok_or_failwith (parse str);;
end

module Message = struct
  type t = {
    postmark : Postmark.t;
    email : Email.t;
  } [@@deriving sexp]

  let to_string message =
    let text = Email.to_string message.email in
    let text = Regexp.(Template.apply escape_line text) in
    String.concat "\n"
    [
      Postmark.to_string message.postmark;
      text;
      ""
    ]
  ;;
end


open Regexp.Infix;;

let unescape line =
  if line =~ Regexp.escaped_line then
    String.chop_prefix_exn line ~prefix:">"
  else
    line
;;

let __UNUSED_VALUE__counting_fold fold ?(from=0) ~init ~f =
    fold
      ~init:(from, init)
      ~f:(fun (n, acc) data -> (n + 1, f n acc data))
;;

module Parser' = struct
  type t = string list * (Postmark.t option)

  type a = string
  type b = Message.t

  let create () = ([], None);;

  let parse ((current, postmark) as t) token =
    let module C = Parser_intf.Comm in

    (* Tries to get a message from the lines read so far *)
    let message_of_t ~next_t = function
      | (current, Some postmark) ->
        let text = String.concat "\n" (List.rev current) in
        begin
        try
          let email = Email.of_octet_stream (Octet_stream.of_string text)
        in
          C.put next_t { Message. postmark = postmark; email = email; }
        with
        Failure s -> C.warning next_t
          ~msg:(Printf.sprintf "ERROR %s in message %s." s (Postmark.to_string postmark))
        end
      | _ -> C.continue next_t
    in
    (* Parses a token (line or EOF) from the parser driver *)
    match token with
    | `Token line ->
      if current = [] && line =~ Regexp.blank then
        C.continue t
      else if Regexp.(line =~ postmark') then
        (* Check if beginning of message *)
        match Postmark.parse line with
        | Result_.Error msg    -> C.warning t ~msg
        | Result_.Ok postmark  -> message_of_t t ~next_t:([], Some postmark)
      else if Option_.is_some postmark then
        (* All non-blank lines must fall inside a message *)
        C.continue ((unescape line) :: current, postmark)
      else
        C.warning t ~msg:(Printf.sprintf "No message context for: %s" line)
    | `Eof       -> message_of_t ~next_t:(create ()) t
  ;;

end

module Parser : Parser_intf.S with type a = string and type b = Message.t
  = Parser_intf.Make (Parser')

module type With_container = sig
  type t

  val t_of_fd : in_channel -> t
  val t_of_file : string -> t
  val of_string : string -> t
  val iter_string : t -> f:(string -> unit) -> unit
  val fold_message : t -> f:('a -> Message.t -> 'a) -> init:'a -> 'a
end

module With_lazy_list =
struct
  type t = Message.t Lazy_list.t

  let t_of_fd fd =
    Parser.parse_lazy_list
      (Lazy_list.of_iterator
        ~init:(fd, input_line fd)
        ~next:(fun (fd, _) -> (fd, input_line fd))
        ~curr:snd)
  ;;

  let t_of_file fname = with_file fname ~f:t_of_fd;;

  let of_string str =
    let line_list = String.split str ~on:'\n' in
    Parser.parse_lazy_list (Lazy_list.of_list line_list)
  ;;

  let iter_string t ~f = Lazy_list.iter t ~f:(Fn_.compose f Message.to_string);;
  let fold_message t ~f ~init = Lazy_list.fold_left ~f ~init t;;
end


(* When creating a Lazy_sequence from an iterator, it can only be traversed once *)
module Lazy_sequence_extra = struct
  let of_fd fd =
    Lazy_sequence.protect (fun () ->
      let rec loop () =
        match input_line fd with
        | Some line -> Lazy_sequence.(line ==> loop)
        | None      -> Lazy_sequence.empty
      in
      loop ())
    ~finally:(fun () -> Pervasives.close_in fd)
  ;;
end

module With_seq = struct
  type t = Message.t Lazy_sequence.t

  let t_of_fd fd = Parser.parse_seq (Lazy_sequence_extra.of_fd fd);;
  let t_of_file fname = Parser.parse_seq (Lazy_sequence.read_lines fname);;

  let of_string str =
    let line_list = String.split str ~on:'\n' in
    Parser.parse_seq (Lazy_sequence.of_list line_list)
  ;;

  let iter_string t ~f =
    Lazy_sequence.iter t ~f:(Fn_.compose f Message.to_string);;
  let fold_message t ~f ~init = Lazy_sequence.fold ~f ~init t;;
end
