(*
 * Copyright (c) 2013-2014 Gregory Tsipenyuk <gregtsip@cam.ac.uk>
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
open Server_config

let try_close cio =
  Printf.printf "closing channel\n%!";
  catch (fun () -> Lwt_io.close cio)
  (function _ -> return ())

let try_close_sock sock =
  Printf.printf "closing socket\n%!";
  catch (fun () ->
    match sock with |None->return()|Some sock->Lwt_unix.close sock)
  (function _ -> return ())

let init_socket addr port =
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_of_string addr, port) in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  Lwt_unix.bind socket sockaddr;
  return socket

let init_unix_socket file =
  let open Lwt_unix in
  (catch (fun () -> mkdir (Filename.dirname file) 0o777))
  (function _ -> return ()) >>= fun () ->
  catch (fun () -> unlink file)
  (function _ -> return ()) >>= fun () ->
  let sockaddr = Unix.ADDR_UNIX file in
  let socket = socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  setsockopt socket Unix.SO_REUSEADDR true;
  bind socket sockaddr;
  catch (fun () ->
  getpwnam "postfix" >>= fun (pw:Lwt_unix.passwd_entry) ->
  chown file pw.pw_uid pw.pw_gid >>= fun () ->
  chmod file  0o777)
  (fun _ -> Printf.printf "warning: postfix is not installed\n%!"; return ()) >>
  return socket

let create_srv_socket addr =
  begin
  match addr with
  | `Inet (addr,port) ->
    init_socket addr port
  | `Unix file ->
    init_unix_socket file
  end >>= fun socket ->
  Lwt_unix.listen socket 10;
  return socket

let create_clnt_socket addr =
  match addr with
  | `Inet (addr,port) ->
    let sockaddr = Unix.ADDR_INET (Unix.inet_addr_of_string addr, port) in
    (Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0,sockaddr)
  | `Unix file ->
    let sockaddr = Unix.ADDR_UNIX file in
    (Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 ,sockaddr)

let accept_ssl sock cert =
  Tls_lwt.accept cert sock >>= fun (channels, addr) ->
  return (None,channels)

let accept_cmn sock =
  Lwt_unix.accept sock >>= fun (sock_c, addr) ->
  let ic = Lwt_io.of_fd ~close:(fun()->return()) ~mode:Lwt_io.input sock_c in
  let oc = Lwt_io.of_fd ~close:(fun()->return()) ~mode:Lwt_io.output sock_c in
  return (Some sock_c,(ic,oc))

let rec accept_conn sock cert =
  catch (fun () ->
  match cert with
  | Some cert -> accept_ssl sock cert
  | None -> accept_cmn sock
  )
  (fun ex ->
  match ex with
  | End_of_file -> accept_conn sock cert
  | _ -> Printf.printf "imaplet: accept_conn exception %s %s\n%!"
    (Printexc.to_string ex) (Printexc.get_backtrace());
    accept_conn sock cert
  )

(* initialize all things *)
let init_all addr config =
  create_srv_socket addr >>= fun sock ->
  if config.ssl then (
    Ssl_.init_ssl config >>= fun cert ->
    return ((Some cert),sock)
  ) else
    return (None,sock)

let starttls config sock () =
  Ssl_.init_ssl config >>= fun cert ->
  Tls_lwt.Unix.server_of_fd
    (Tls.Config.server ~certificates:cert ()) sock >>= fun srv ->
  return (Tls_lwt.of_t srv)

let client_send addr f =
  let open Lwt_unix in
  let (socket,sockaddr) = create_clnt_socket addr in
  Lwt_unix.connect socket sockaddr >>
  let inchan = Lwt_io.of_fd ~mode:Lwt_io.input socket in
  let outchan = Lwt_io.of_fd ~mode:Lwt_io.output socket in
  f inchan outchan >>
  Lwt_unix.close socket >>
  try_close inchan >> try_close outchan

(**
 * start accepting connections
 **)
let server addr config f err =
  let open Server_config in
  init_all addr config >>= fun (cert,sock) ->
  let rec connect f sock cert =
    accept_conn sock cert >>= fun (sock_c,(netr,netw)) ->
    Printf.printf "accepted client connection\n%!";
    async ( fun () ->
      catch( fun () ->
        f sock_c netr netw >>
        try_close netr >> try_close netw >> try_close_sock sock_c
      ) 
      (function ex -> (err ex) >> try_close netr >> try_close netw >> try_close_sock sock_c)
    ); connect f sock cert
  in
  connect f sock cert
