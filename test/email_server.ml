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

exception InvalidCommand

let sock_path = Filename.concat Install.data_path "sock/smtp"
let imap_addr = Unix.ADDR_UNIX sock_path

let rec args i port smtp =
  if i >= Array.length Sys.argv then
    port,smtp
  else
    match Sys.argv.(i) with 
    | "-port" -> args (i+2) (int_of_string Sys.argv.(i+1)) smtp
    | "-smtp" -> args (i+1) port true
    | _ -> raise InvalidCommand

let commands f =
  try 
    let port,smtp = args 1 3587 false in
    f port smtp
  with 
    | InvalidCommand -> Printf.printf "usage: email_server -port [port] -smtp\n%!"; Pervasives.exit 0
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

let smtp_server port =
  let mutex = Lwt_mutex.create () in
  Lwt_mutex.lock mutex >>= fun () ->
  catch (fun() ->
    let send_stats = ref 0. in
    Lwt_unix.gethostname () >>= fun host ->
    Lwt_io.open_connection imap_addr >>= fun (imap_ic,imap_oc) ->
    let sockaddr = Unix.ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", port) in
    let server = Lwt_io.establish_server ~backlog:10 sockaddr
    (fun (ic,oc) ->
      ignore_result (
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
      )
    ) in
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

let append_re = Re_posix.compile_pat ~opts:[`ICase] 
  "^a lappend ([^ ]+)( ([^ ]+))? INBOX [{]([0-9]+)\\+[}]$"

(* 'imap server' *)
let imap_server () =
  let mutex = Lwt_mutex.create () in
  Lwt_mutex.lock mutex >>= fun () ->
  catch (fun () ->
  let (strm,push_append_strm) = Lwt_stream.create () in
  async (fun () -> Amailbox.async_append strm);
  let server = Lwt_io.establish_server ~backlog:10 imap_addr
  (fun (ic,oc) ->
    ignore_result (
      let rec loop cnt =
        Lwt_io.read_line_opt ic >>= function
        | None -> return ()
        | Some command ->
        let subs = Re.exec append_re command in
        let user = Re.get subs 1 in
        let password = try Some (Re.get subs 3) with Not_found -> None in
        let size = int_of_string (Re.get subs 4) in
        Account.authenticate_user user ?password () >>= fun (_,_,_,c) ->
        let amailbox = Amailbox.create (get_user_config Server_config.srv_config c) user password in
        begin
        Amailbox.append amailbox "INBOX" ic oc push_append_strm None None None 
         (Imaplet_types.LiteralPlus size) >>= function
        | `Ok -> Printf.printf "created message %d\n%!" cnt; return ()
        | _ -> Printf.printf "failed\n%!"; return ()
        end >>= fun () ->
        Lwt_io.write_line oc "a OK APPEND completed" >>= fun () -> loop (cnt + 1) 
      in
      loop 1 
    )
  ) in
  let sigh = Lwt_unix.on_signal 10 (fun s -> Lwt_mutex.unlock mutex) in
  Lwt_mutex.lock mutex >>= fun () ->
  Lwt_io.shutdown_server server;
  return ()
  ) (fun ex -> Printf.printf "imap_server exception: %s\n%!" (Printexc.to_string ex); return ())

let start_smtp port smtp =
  Lwt_unix.sleep 1. >>= fun () ->
  let pid = Lwt_unix.fork () in
  if pid = 0 then (
    if smtp then (
      Unix.execv Configuration.smtp_srv_exec [|Configuration.smtp_srv_exec|]
    ) else (
      smtp_server port >>= fun () ->
      return 0
    )
  ) else
    return pid

let start_imap () =
  let pid = Lwt_unix.fork () in
  if pid = 0 then (
    imap_server () >>= fun () ->
    return 0
  ) else
    return pid

let () =
commands (fun port smtp ->
  Lwt_main.run (
    Lwt_unix.system ("rm -rf " ^ sock_path) >>= fun _ ->
    start_imap () >>= fun imap_pid ->
    start_smtp port smtp >>= fun smtp_pid ->
    Lwt_io.read_line Lwt_io.stdin >>= fun _ ->
    Lwt_unix.system (Printf.sprintf "kill -9 %d" smtp_pid) >>= fun _ ->
    Lwt_unix.system (Printf.sprintf "kill -9 %d" imap_pid) >>= fun _ ->
    return ()
  )
)
