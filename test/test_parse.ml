open Lwt
open Sexplib
open Imaplet
open Commands
open Parsemail
open Mailbox

let pr message msg = 
  let len = String.length message in
  let offset = 0 in
  let prfx = if len > 50 then String.sub message offset 50 else message in
  let pstfx = if len > 100 then String.sub message (offset+len - 50) 50 else "" in
  Printf.printf "  %s: %d %s...%s\n%!" msg len prfx pstfx

let get_hdr_attrs headers =
  List.fold_left (fun (rfc822,content_type) (n,v) ->
    if Re.execp (Re_posix.compile_pat ~opts:[`ICase] "Content-Type") n then (
      let content_type = 
        try 
          let subs = Re.exec (Re_posix.compile_pat "^[ ]*([^ \t;]+)") v in
          Re.get subs 1
        with Not_found -> v
      in
      let rfc822 =
        if Re.execp (Re_posix.compile_pat ~opts:[`ICase] "message/rfc822") v then
          true
        else
          false
      in
      rfc822,content_type
    ) else
      rfc822,content_type
  ) (false,"text/plain") headers

let email_raw_content email =
  match (Email.raw_content email) with
  | Some rc -> Octet_stream.to_string rc
  | None -> ""

let headers_to_string headers =
  String_monoid.to_string (Header.to_string_monoid headers)

let rec walk email multipart =
  let headers = Header.to_list (Email.header email) in
  let headers_s = headers_to_string (Email.header email) in
  let rfc822,content_type = get_hdr_attrs headers in
  let content = email_raw_content email in
  pr headers_s "Headers";
  match Email.content email with
  | `Data _ ->
    if multipart && rfc822 then (
      let email = Email.of_string content in
      pr content "Message";
      walk email multipart
    ) else (
      pr content "Data";
    )
  | `Message _ -> assert (false)
  | `Multipart elist ->
    pr content "Multipart";
    List.iteri (fun cnt email ->
      let content = Email.to_string email in
      pr content (Printf.sprintf "Part %d" cnt);
      walk email true;
    ) elist

let () =
  Lwt_main.run (
    Utils.fold_email_with_file1 Sys.argv.(1) (fun cnt message ->
      Printf.fprintf stderr "%d\r%!" cnt;
      let postmark = message.postmark in
      let email = message.email in
      Printf.printf "--> start\n%!";
      Printf.printf "Full message %d\n%!" (String.length (Message.to_string message));
      pr (Postmark.to_string postmark) "Postmark";
      pr (Email.to_string email) "Email";
      walk email false;
      Printf.printf "<-- end\n%!";
      return (`Ok (cnt+1))
    ) 1 >>= fun _ ->
    return ()
  )
