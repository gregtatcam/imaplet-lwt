module Debug_in_this_directory = Debug
module Debug = Debug_in_this_directory
open Sexplib.Std
open Core_replace
open Lazys


type t = string [@@deriving sexp]
let create = Fn_.id;;


module Generator = struct
  let bcharnospace =
    "abcdefghijklmnopqrstuvwxyz" ^
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ^
    "0123456789" ^
    "'()+_,-./:=?"
  ;;
  let bcharnospace_len = String.length bcharnospace

  let __UNUSED_VALUE__min_len = 1
  let max_len = 70
  let suffix_len = 20
  let base_len = max_len - suffix_len

  let __UNUSED_VALUE__max_tries = 1000

  let init_random_once = Lazy_m.of_fun (fun () -> Random.self_init ())

  let random_seq len =
    Lazy_m.force init_random_once;
    let str = String.create len in
    let rec loop pos =
      if pos < len then
      begin
        str.[pos] <- bcharnospace.[Random.int bcharnospace_len];
        loop (pos + 1)
      end
      else
        str
    in
    loop 0
  ;;

  (* This prefix guarantees that the boundary will not apear in
    - Headers
    - Quoted-printable text
    - Base64 encoded content.

    The only posibility is that it might appear in plaintext, but
    that would be incredibly rare when using a good random number
    generator.
    *)
  let prefix = "--=_::"
  let prefix_len = String.length prefix

  let generate_raw ?(validate=(Fn_.const true)) len =
    let rec generate () =
      let boundary = prefix ^ (random_seq (len - prefix_len)) in
      if validate boundary then
        generate ()
      else
        boundary
    in
    generate ()
  ;;

  let generate ?text ?suggest () =
    ignore text;
    match suggest with
    | Some suggestion -> suggestion
    | None -> create (generate_raw max_len)
  ;;

  let generate_list ?text () =
    ignore text;
    let base = generate_raw base_len in
    Lazy_sequence.init
      (fun i -> Some (create (Printf.sprintf "%s::%016x" base i)))
  ;;
end

let generate = Generator.generate;;
let generate_list = Generator.generate_list;;


module Open = struct

  let to_string_monoid t =
    String_monoid.concat_string ["\n"; "--"; t; "\n"]
  ;;

end

module Close = struct

  let to_string_monoid t =
    String_monoid.concat_string ["\n"; "--"; t; "--"]
  ;;
end

module Open_first = struct

  let to_string_monoid t = String_monoid.concat_string ["--"; t; "\n"];;

end

let of_string = Fn_.id;;
let to_string = Fn_.id;;

let __UNUSED_VALUE__of_octet_streams _l = generate ();;

let split_octet_stream t ~octet_stream =
  let lexbuf = Octet_stream.to_lexbuf octet_stream in
  let rec loop pos acc has_prologue =
    let sub ?stop () =
      let len = Option_.map stop ~f:(fun stop -> stop - pos) in
      Octet_stream.sub ~pos ?len octet_stream in
    match Lexer.find_boundary t lexbuf with
    | `Open_boundary_first pos ->
      loop pos acc false
    | `Open_boundary (stop, pos) ->
      let chunk = sub ~stop () in
      loop pos (chunk :: acc) has_prologue
    | `Close_boundary (stop, pos) ->
      let chunk = sub ~stop () in
      let epilogue = Octet_stream.sub ~pos octet_stream in
      (chunk :: acc, Some epilogue, has_prologue)
    | `Eof ->
      Debug.run_debug (fun () -> Printf.eprintf "Warning: No close boundary found\n");
      let chunk = sub () in (chunk :: acc, None, has_prologue)
  in
  (* RFC 2046: A multipart body may have a prologue and an epilogue *)
  let parts, epilogue, has_prologue = (loop 0 [] true) in
  match List.rev parts with
  | [] -> (Some octet_stream, [], epilogue)
  | (prologue :: parts) as parts' ->
      if has_prologue then
        (Some prologue, parts, epilogue)
      else
        (None, parts', epilogue)
;;

