open Lwt
open Commands
open Commands.Utils
open Commands.Lazy_message
open Parsemail
open Commands.Email_parse

module MapStr = Map.Make(String)

exception InvalidCommand

let get map key =
  MapStr.find key !map

let key s n =
  (string_of_int n) ^ "-" ^ s

let email_content email =
  match (Email.raw_content email) with
  | None -> ""
  | Some content -> Octet_stream.to_string content

let match_re ~str ~re =
  try
    let _ = Str.search_forward (Str.regexp_case_fold re) str 0 in true
  with Not_found -> false

let message_rfc822 email =
  let headers = Header.to_list (Email.header email) in
  List.exists (fun (n,v) -> 
    (String.lowercase n) = "content-type" && match_re ~str:v ~re:"message/rfc822" 
  ) headers

let rec walk email part =
  begin
  match part with
  | None -> ()
  | Some part -> 
    Printf.fprintf stderr "---------------------------------------- part %d\n%!" part
  end;
  Printf.fprintf stderr "---------------------------------------- header\n%!";
  Printf.fprintf stderr "%s" 
  (String_monoid.to_string (Header.to_string_monoid (Email.header email)));
  match (Email.content email) with
  | `Data content ->
    if part <> None && message_rfc822 email then (
      Printf.fprintf stderr "---------------------------------------- message\n%!";
      walk (Email.of_string (email_content email)) None
    ) else (
      Printf.fprintf stderr "---------------------------------------- content\n%!";
      Printf.fprintf stderr "%s" (email_content email);
    )
  | `Message message ->
    Printf.fprintf stderr "---------------------------------------- message\n%!";
    walk message None
  | `Multipart lpart ->
    Printf.fprintf stderr "---------------------------------------- multipart %d\n%!" (List.length lpart);
    List.iteri (fun i email -> walk email (Some i)) lpart

let get_attachments config map =
  MapStr.fold (fun key data m ->
    let key = Regex.replace ~regx:"^[0-9]+-" ~tmpl:"" key in
    if Regex.match_regex ~regx:"^postmark\\|headers\\|content" key = false then
      MapStr.add key (Lazy.from_fun (fun () -> do_decrypt config data)) m
    else
      m
  ) map MapStr.empty

let rec args i mbox lzy unique =
  if i >= Array.length Sys.argv then
    mbox,lzy,unique 
  else
    match Sys.argv.(i) with 
    | "-archive" -> args (i+2) Sys.argv.(i+1) lzy unique 
    | "-lazy" -> args (i+1) mbox true unique 
    | "-unique" -> args (i+2) mbox lzy (Some (int_of_string Sys.argv.(i+1)))
    | _ -> raise InvalidCommand

let usage () =
  Printf.fprintf stderr "usage: parse_restore -archive filename -lazy -unique number\n%!"

let commands f =
  try 
    let mbox,lzy,unique = args 1 "" false None in
    if mbox = "" then
      raise InvalidCommand
    else
      try 
        f mbox lzy unique
      with ex -> Printf.printf "%s\n%!" (Printexc.to_string ex)
  with _ -> usage ()

let fill file push_stream =
  let add_message buffer push_stream =
    if Buffer.length buffer > 0 then (
      let wseq = Mailbox.With_seq.of_string (Buffer.contents buffer) in
      Mailbox.With_seq.fold_message wseq ~f:(fun _ message ->
        push_stream (Some message);
      ) ~init:()
    ) else (
      ()
    )
  in
  Lwt_io.with_file ~mode:Lwt_io.Input file (fun ic ->
    let buffer = Buffer.create 10000 in
    let rec lines ic buffer = 
      Lwt_io.read_line_opt ic >>= function
      | Some line ->
      let line = line ^ "\n" in
      let regx = "^from [^ ]+ " ^ Regex.dayofweek ^ " " ^ Regex.mon ^ " " ^
        "[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [0-9]+" in
      if Regex.match_regex line ~case:false ~regx && Buffer.length buffer > 0 then (
        add_message buffer push_stream;
        Buffer.clear buffer;
        Buffer.add_string buffer line;
        lines ic buffer 
      ) else (
        Buffer.add_string buffer line;
        lines ic buffer 
      )
      | None -> add_message buffer push_stream; return ()
    in
    lines ic buffer 
  ) >>= fun () -> push_stream None; return ()

exception Done

let fold_messages strm unique f =
  Lwt_stream.to_list strm >>= fun l ->
  let rec fold cnt =
    catch (fun () ->
      Lwt_list.fold_left_s (fun cnt message ->
        f message cnt >>= fun _ ->
        match unique with
        | Some stop -> if cnt >= stop then raise Done; return (cnt + 1)
        | None -> return (cnt + 1)
      ) cnt l >>= fun cnt ->
      match unique with
      | Some _ -> fold cnt
      | None -> return ()
    ) (function | Done -> return ()| ex -> raise ex)
  in
  fold 1

let transform cnt = function
  | `Postmark p -> Str.global_replace (Str.regexp "@") ((string_of_int cnt) ^ "@") p
  | `Headers h -> h ^ "X-Imaplet-unique: " ^ (string_of_int cnt) ^ "\n"
  | `Body b -> b ^ "\n" ^ (string_of_int cnt) ^ "\n"
  | `Attachment a -> a ^ (string_of_int cnt)

let default_transform = function
  | `Postmark p -> p
  | `Headers h -> h
  | `Body b -> b
  | `Attachment a -> a

let () =
  commands (fun file lzy unique ->
    Lwt_main.run (
      let config = Server_config.srv_config in
      let (strm,push_stream) = Lwt_stream.create () in
      async (fun () -> 
        fill file push_stream 
      );
      let map = ref MapStr.empty in
      fold_messages strm unique (fun message cnt ->
        Printf.fprintf stderr "---------------------------------------- new message %d\n%!" cnt;
        (*walk message.email None;*)
        let tr = (if unique = None then default_transform else transform cnt) in
        parse ~transform:tr config message ~save_message:(fun postmark headers content ->
          map := MapStr.add (key "postmark" cnt) postmark !map;
          map := MapStr.add (key "headers" cnt) headers !map;
          map := MapStr.add (key "content" cnt) content !map;
          return ()
        )
        ~save_attachment:(fun contid attachment ->
          map := MapStr.add (key contid cnt) attachment !map;
          return ()
        ) >>= fun () ->
        (if lzy then (
          let postmark = get map (key "postmark" cnt) in
          let headers = get map (key "headers" cnt) in
          let content = get map (key "content" cnt) in
          do_decrypt config postmark >>= fun postmark ->
          do_decrypt_headers config headers >>= fun (m,headers) ->
          do_decrypt_content config content >>= fun content ->
          let (module LE:LazyEmail_inst) = build_lazy_email_inst
            (module Irmin_core.LazyIrminEmail)
            (
              m,
              headers,
              Lazy.from_fun (fun () -> return content),
              get_attachments config !map
            ) in
          LE.LazyEmail.to_string LE.this >>= fun str ->
    
          Printf.printf "%s\n%s\n%!" postmark str;
          return ()
        ) else (
          restore config ~get_message:(fun () -> 
           let postmark = get map (key "postmark" cnt) in
           let headers = get map (key "headers" cnt) in
           let content = get map (key "content" cnt) in
           return (postmark,headers,content)
          )  
          ~get_attachment:(fun contid -> 
            return (get map (key contid cnt))
          ) >>= fun message ->
          Printf.printf "%s" (Mailbox.Message.to_string message);
          return ()
        )) >>= fun () ->
  
        map := MapStr.empty;
        return (cnt+1)
      ) >>= fun _ ->
      return ()
    )
  )
