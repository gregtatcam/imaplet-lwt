open Lwt
open Email_message
open Utils

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

let () =
  Lwt_main.run (
    let wseq = Mailbox.With_seq.t_of_file Sys.argv.(1) in
    let map = ref MapStr.empty in
    Mailbox.With_seq.fold_message wseq ~f:(fun cnt message ->
      Printf.fprintf stderr "---------------------------------------- new message\n%!";
      (*walk message.email None;*)
      cnt >>= fun cnt ->
      Email_parse.parse message ~save_message:(fun postmark headers content ->
        map := MapStr.add (key "postmark" cnt) postmark !map;
        map := MapStr.add (key "headers" cnt) headers !map;
        map := MapStr.add (key "content" cnt) content !map;
        return ()
      )
      ~save_attachment:(fun contid attachment ->
        map := MapStr.add (key contid cnt) attachment !map;
        return ()
      ) >>
      Email_parse.restore ~get_message:(fun () -> 
        let postmark = get map (key "postmark" cnt) in
        let headers = get map (key "headers" cnt) in
        let content = get map (key "content" cnt) in
        return (postmark,headers,content)
      )  
      ~get_attachment:(fun contid -> 
        return (get map (key contid cnt))
      ) >>= fun message ->
      Printf.printf "%s" (Mailbox.Message.to_string message);
      map := MapStr.empty;
      return (cnt+1)
    ) ~init:(return (0)) >>= fun _ ->
    return ()
  )
