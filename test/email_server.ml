(*
 * Copyright (c) 2013-2016 Gregory Tsipenyuk <gregtsip@cam.ac.uk>
 * 
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Lwt
open Imaplet
open Commands
open Imaplet_types

exception InvalidCommand

let sock_path = Filename.concat Install.data_path "sock/smtp"
let imap_addr = function
  | None -> Unix.ADDR_UNIX sock_path
  | Some port -> Unix.ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", port)

let get_smtp = function
  | "internal" -> `Int
  | "external" -> `Ext
  | "none" -> `None
  | _ -> raise InvalidCommand

let rec args i imapport smtpport smtp =
  if i >= Array.length Sys.argv then
    imapport,smtpport,smtp
  else
    match Sys.argv.(i) with 
    | "-smtp-port" -> args (i+2) imapport (int_of_string Sys.argv.(i+1)) smtp
    | "-imap-port" -> args (i+2) (Some (int_of_string Sys.argv.(i+1))) smtpport smtp
    | "-smtp" -> args (i+2) imapport smtpport (get_smtp (Sys.argv.(i+1)))
    | _ -> raise InvalidCommand

let commands f =
  try 
    let imapport,smtpport,smtp = args 1 None 3587 `None in
    f imapport smtpport smtp
  with 
    | InvalidCommand -> Printf.printf 
      "usage: email_server -imap-port [port] -smtp-port [port] -smtp [internal|external|none]\n%!"; 
      Pervasives.exit 0
    | ex -> Printf.printf "%s\n%!" (Printexc.to_string ex); Pervasives.exit 0

let write_file ?(append=false) file content =
  let open Unix in
  let flags = [O_NONBLOCK;O_WRONLY;O_CREAT] in
  let flags = if append then O_APPEND :: flags else flags in
  Lwt_io.with_file ~flags ~mode:Lwt_io.output file (fun oc -> 
    Lwt_io.write oc content)

let read_file file =
  let open Unix in
  Lwt_io.with_file ~flags:[O_NONBLOCK;O_RDONLY] ~mode:Lwt_io.input file (fun ic ->
    Lwt_io.read ic)

let smtp_server port imapport =
  let mutex = Lwt_mutex.create () in
  Lwt_mutex.lock mutex >>= fun () ->
  catch (fun() ->
    let send_stats = ref 0. in
    Lwt_unix.gethostname () >>= fun host ->
    Lwt_io.open_connection (imap_addr imapport) >>= fun (imap_ic,imap_oc) ->
    let sockaddr = Unix.ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", port) in
    Lwt_io.establish_server_with_client_address ~backlog:10 sockaddr
    (fun _ (ic,oc) ->
      let rec loop user is_data =
        Lwt_io.read_line_opt ic >>= function
        | None -> return ()
        | Some l ->
          if is_data then (
            let buffer = Buffer.create 100 in
            let rec get_message () =
              Lwt_io.read_line_opt ic >>= function
              | None -> return ()
              | Some l ->
                if l = "." then (
                  return ()
                ) else (
                  Buffer.add_string buffer l;
                  Buffer.add_string buffer "\n";
                  get_message () 
                )
            in
            get_message () >>= fun () ->
            let t = Unix.gettimeofday () in
            Printf.printf "data collected %.04f\n%!" (t -. !send_stats);
            send_stats := t;
            Lwt_io.write_line oc "250" >>= fun () ->
            Lwt_io.write imap_oc 
                (Printf.sprintf "a lappend %s INBOX {%d+}\n" user (Buffer.length buffer)) >>= fun () ->
            Lwt_io.write imap_oc (Buffer.contents buffer) >>= fun () ->
            Lwt_io.read_line imap_ic >>= fun _ -> (* a OK APPEND completed *)
            let t = Unix.gettimeofday () in
            Printf.printf "data written to imap %.04f\n%!" (t -. !send_stats);
            send_stats := t;
            loop "" false
          ) else (
            let re = Re_posix.compile_pat ~opts:[`ICase] 
              "^(helo|ehlo|mail from:|rcpt to:|data|quit)(.*)$" in
            try
              let subs = Re.exec re l in
              let cmd = String.lowercase (Re.get subs 1) in
              match cmd with
              | "helo" -> Lwt_io.write_line oc ("250 " ^ host) >>= fun () -> loop user false
              | "mail from:" -> Lwt_io.write_line oc "250" >>= fun () -> loop user false
              | "rcpt to:" -> 
                let arg = Re.get subs 2 in
                let re = Re_posix.compile_pat "^[ ]*<?([^ @<>]+)(@([^> ]+))?>?[ ]*$" in
                let subs = Re.exec re arg in
                let user = Re.get subs 1 in
                Account.authenticate_user user () >>= fun (_,_,_,c) ->
                Lwt_io.write_line oc "250" >>= fun () -> loop user false
              | "ehlo" -> 
                Lwt_io.write_line oc ("250-" ^ host) >>= fun () ->
                Lwt_io.write_line oc ("250 SIZE 15360000") >>= fun () ->
                loop user false
              | "data" -> 
                let t = Unix.gettimeofday () in
                Printf.printf "start data collection %.04f\n%!" (t -.  !send_stats);
                send_stats := t;
                Lwt_io.write_line oc "354" >>= fun () -> loop user true
              | "quit" -> Lwt_io.write_line oc "221"
              | _ -> loop user false
            with Not_found -> loop user false
          )
      in
      Lwt_io.write_line oc ("220 " ^ host) >>= fun () ->
      loop "" false
    ) >>= fun server ->
    let sigh = Lwt_unix.on_signal 10 (fun s -> Lwt_mutex.unlock mutex) in
    Lwt_mutex.lock mutex >>= fun () -> 
    Lwt_io.close imap_oc >>= fun () ->
    Lwt_io.close imap_ic >>= fun () ->
    Lwt_io.shutdown_server server;
    return ()
  ) (fun ex -> Printf.printf "smtp_server exception: %s\n%!" (Printexc.to_string ex); return ())

let get_user_config config = 
  let open Server_config in
  let open Account in
  function
  | Some c ->
      {config with data_store = c.acct_data_store; encrypt = c.acct_encrypt;
      compress = c.acct_compress; compress_attach = c.acct_compress_attach;
      auth_required = c.acct_auth_required; maildir_parse = c.acct_maildir_parse; 
      single_store = c.acct_single_store; hybrid = c.acct_hybrid;
      compress_repo = c.acct_compress_repo}
  | None -> config

let get_command command =
  Printf.fprintf stderr "%s\n%!" command;
  try
  let command = Parser.request (Lex.read (ref `Tag)) 
    (Lexing.from_string (command^"\r\n")) in
  let tag = command.tag in
  let command =
  match command.command with
  | Any cmd ->
    begin
    match cmd with
    | Cmd_Logout -> `Logout
    | _ -> `Bad
    end
  | Notauthenticated cmd ->
    begin
    match cmd with
    | Cmd_Login (user,password) -> `Login (user,password)
    | Cmd_Lappend (user,password,mailbox,literal) -> 
      `Lappend (user,password,mailbox,literal)
    | _ -> `Bad
    end
  | Authenticated cmd ->
    begin
    match cmd with
    | Cmd_Select (mailbox,_) -> `Select mailbox
    | Cmd_Append (mailbox,flags,date,literal) ->
      `Append (mailbox,flags,date,literal)
    | _ -> `Bad
    end
  | Selected cmd ->
    begin
    match cmd with
    | Cmd_Fetch (sequence,fetch,condstore,buid) ->
      `Fetch (sequence,fetch,condstore,buid)
    | Cmd_Close -> `Close
    | _ -> `Bad
    end
  | Done -> `Logout
  in
  (tag,command)
  with Parser.Error -> 
    ("*", `Bad)

(* 'imap server' *)
let imap_server port =
  let mutex = Lwt_mutex.create () in
  Lwt_mutex.lock mutex >>= fun () ->
  catch (fun () ->
  let push_append_strm = None in
  (*let (strm,push_append_strm) = Lwt_stream.create () in
  async (fun () -> Amailbox.async_append strm);*)
  Lwt_io.establish_server_with_client_address ~backlog:10 (imap_addr port)
  (fun _ (ic,oc) ->
      let sprf = Printf.sprintf in
      let response ?(tag="") message =
        let buff = if tag = "" then message else (tag ^ " " ^ message) in
        Lwt_io.write_line oc buff >>= fun () ->
        Lwt_io.flush oc
      in
      let create ?password user =
        Account.authenticate_user user ?password () >>= fun (_,_,a,c) ->
        let amailbox =
          Amailbox.create (get_user_config Server_config.srv_config c) user password
        in
        return (a,amailbox)
      in
      let append tag amailbox mailbox flags date literal =
        Amailbox.append amailbox mailbox ic oc push_append_strm
          None flags date literal >>= function
        | `Ok -> response ~tag "OK APPEND completed"
        | _ -> response ~tag "NO"
      in
      let try_close c = catch (fun() -> Lwt_io.close c)(fun _ -> return()) in
      response ("* OK [CAPABILITY IMAP4rev1 LITERAL+ SASL-IR LOGIN-REFERRALS ID " ^
        "ENABLE IDLE STARTTLS AUTH=PLAIN] Imaplet ready.") >>= fun () ->
      let rec loop ?(user="") ?(password="") amailbox =
        Lwt_io.read_line_opt ic >>= function
        | None -> try_close ic >>= fun () -> try_close oc
        | Some command ->
            let (tag,command) = get_command command in
        match command with
        | `Bad -> response ~tag:"a" "BAD"
        | `Logout -> 
          Printf.fprintf stderr "logout\n%!";
          try_close ic >>= fun () -> try_close oc
        | `Login (user,password) ->
          create ~password user >>= fun (authenticated,mbox) ->
          let (mbox,msg) = 
          if authenticated then (
            (mbox, "OK LOGIN completed")
          ) else (
            (amailbox, "NO LOGIN failed")
          )
          in
          response ~tag msg  >>= fun () ->
          loop ~user ~password mbox
        | `Select mailbox ->
          begin
          Amailbox.select amailbox mailbox >>= function
          | `Ok (selected, metadata) ->
            response (sprf "* OK [UIDVALIDITY %s]" metadata.uidvalidity) >>= fun () ->
            response (sprf "* OK [UIDNEXT %d]" metadata.uidnext) >>= fun () ->
            response (sprf "* %d EXISTS" metadata.count) >>= fun () ->
            response (sprf "* %d RECENT" metadata.recent) >>= fun () ->
            return ("OK SELECT completed",selected)
          | `NotExists -> return ("NO, doesn't exist",amailbox)
          | `NotSelectable -> return ("NO, not selectable",amailbox)
          end >>= fun (msg,amailbox) ->
          response ~tag msg >>= fun () ->
          loop ~user ~password amailbox
        | `Append (mailbox,flags,date,literal) ->
          append tag amailbox mailbox flags date literal >>= fun () -> 
          loop ~user ~password amailbox
        | `Lappend (user,password,mailbox,literal) ->
          create ?password user >>= fun (_,mbox) ->
          append tag mbox mailbox None None literal >>= fun () -> 
          loop ~user ?password amailbox
        | `Fetch (sequence,fetch,condstore,buid) ->
          begin
          let t = Unix.gettimeofday() in
          Amailbox.fetch amailbox (fun()->return()) 
            (fun v -> 
              let l = List.concat [["* "];v;["\r\n"]] in
              Lwt_list.iter_s (Lwt_io.write oc) l
            ) sequence fetch condstore buid >>= function
          | `Ok ->
            let diff = Unix.gettimeofday() -. t in
            return (sprf "OK FETCH completed, %.04f sec" diff)
          | `NotExists -> return "NO, doesn't exist"
          | `NotSelectable -> return "NO, not selectable"
          | `Error msg -> return (sprf "NO, %s" msg)
          end >>= fun msg ->
          response ~tag msg >>= fun () ->
          loop ~user ~password amailbox
      in
      loop (Amailbox.empty())
  ) >>= fun server ->
  let sigh = Lwt_unix.on_signal 10 (fun s -> Lwt_mutex.unlock mutex) in
  Lwt_mutex.lock mutex >>= fun () ->
  Lwt_io.shutdown_server server;
  return ()
  ) (fun ex -> Printf.printf "imap_server exception: %s\n%!" (Printexc.to_string ex); return ())

let start_smtp port imapport smtp =
  match smtp with
  | `None -> return 0
  | `Int | `Ext ->
  Lwt_unix.sleep 1. >>= fun () ->
  let pid = Lwt_unix.fork () in
  if pid = 0 then (
    if smtp = `Ext then (
      Unix.execv Configuration.smtp_srv_exec [|Configuration.smtp_srv_exec|]
    ) else (
      smtp_server port imapport >>= fun () ->
      return 0
    )
  ) else
    return pid

let start_imap port =
  let pid = Lwt_unix.fork () in
  if pid = 0 then (
    imap_server port >>= fun () ->
    return 0
  ) else
    return pid

let () =
commands (fun imapport smtpport smtp ->
  let _ = Gc.create_alarm (fun() -> Printf.fprintf stderr "Finished GC cycle\n%!") in
  Lwt_main.run (
    Lwt_unix.system ("rm -rf " ^ sock_path) >>= fun _ ->
    start_imap imapport >>= fun imap_pid ->
    start_smtp smtpport imapport smtp >>= fun smtp_pid ->
    Lwt.pick [
      (Lwt_unix.wait () >>= fun _ -> 
      Printf.fprintf stderr "got child signal\n%!";
      return ());
      (Lwt_io.read_line Lwt_io.stdin >>= fun _ ->
      Lwt_unix.system (Printf.sprintf "kill -9 %d" smtp_pid) >>= fun _ ->
      Lwt_unix.system (Printf.sprintf "kill -9 %d" imap_pid) >>= fun _ ->
      return ())
    ]
  )
)
