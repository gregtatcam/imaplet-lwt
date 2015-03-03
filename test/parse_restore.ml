open Lwt
open Imaplet_email
open Utils
open Lazy_message
open Email_parse

module MapStr = Map.Make(String)

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

let get_arg () =
  if Sys.argv.(1) = "-lazy" then
    true,Sys.argv.(2)
  else
    false,Sys.argv.(1)

let () =
  Lwt_main.run (
    let config = Server_config.srv_config in
    let (lzy,file) = get_arg() in
    let wseq = Mailbox.With_seq.t_of_file file in
    let map = ref MapStr.empty in
    Mailbox.With_seq.fold_message wseq ~f:(fun cnt message ->
      Printf.fprintf stderr "---------------------------------------- new message\n%!";
      (*walk message.email None;*)
      cnt >>= fun cnt ->
      parse config message ~save_message:(fun postmark headers content ->
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
  
        Printf.printf "%s\n%s%!" postmark str;
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
    ) ~init:(return (0)) >>= fun _ ->
    return ()
  )