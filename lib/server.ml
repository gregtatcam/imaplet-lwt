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
open Commands

let try_close cio =
  catch (fun () -> Lwt_io.close cio)
  (function _ -> return ())

let try_close_sock sock =
  catch (fun () -> 
    match sock with |None->return()|Some sock->Lwt_unix.close sock)
  (function _ -> return ())

let msgt_to_str = function
  | `Smtp -> "smtp"
  | `Client -> "client"

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
  chmod file 0o777 >>
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

let accept_ssl msgt sock cert =
  Tls_lwt.accept cert sock >>= fun (channels, addr) ->
  return (msgt,None,channels)

let accept_cmn msgt sock =
  Lwt_unix.accept sock >>= fun (sock_c, addr) ->
  let ic = Lwt_io.of_fd ~close:(fun()->return()) ~mode:Lwt_io.input sock_c in
  let oc = Lwt_io.of_fd ~close:(fun()->return()) ~mode:Lwt_io.output sock_c in
  return (msgt,Some sock_c,(ic,oc))

let rec accept_conn msgt sock cert = 
  Log_.log `Debug "imaplet: accepting connections\n";
  catch (fun () ->
  match cert with
  | Some cert -> accept_ssl msgt sock cert
  | None -> accept_cmn msgt sock
  )
  (fun ex ->
  match ex with
  | End_of_file -> accept_conn msgt sock cert
  | _ -> Log_.log `Error (Printf.sprintf "imaplet: accept_conn exception %s %s %s\n"
    (msgt_to_str msgt) (Printexc.to_string ex) (Printexc.get_backtrace())); 
    accept_conn msgt sock cert
  )

(* init local delivery *)
let init_local_delivery () =
  let pid = Unix.fork() in
  if pid <> 0 then
    ()
  else (
    Unix.execv Configuration.smtp_srv_exec [|Configuration.smtp_srv_exec|]
  )

(* initialize all things *)
let init_all config =
  let open Server_config in
  create_srv_socket (`Inet (config.addr,config.port)) >>= fun sock ->
  create_srv_socket (`Unix (Filename.concat config.data_path "sock/smtp")) >>= fun unix_sock ->
  init_local_delivery ();
  if config.ssl then (
    Ssl_.init_ssl config >>= fun cert ->
    return ((Some cert),sock,unix_sock)
  ) else
    return (None,sock,unix_sock)

let init_connection msgt w =
  catch( fun () ->
    match msgt with
    | `Smtp -> return ()
    | `Client ->
      let resp = "* OK [CAPABILITY " ^ Configuration.capability ^ "] Imaplet ready.\r\n" in
      Lwt_io.write w resp >>
      Lwt_io.flush w
  )
  (fun ex ->
    Log_.log `Error (Printf.sprintf "imaplet: exception writing initial capability %s %s\n" 
      (msgt_to_str msgt) (Printexc.to_string ex));
    return ()
  )

let starttls config sock () =
  Ssl_.init_ssl config >>= fun cert ->
  Tls_lwt.Unix.server_of_fd
    (Tls.Config.server ~certificates:cert ())
    (Utils.option_value_exn sock) >>= fun srv ->
  return (Tls_lwt.of_t srv)

(**
 * start accepting connections
**)
let create config =
  let open Connections in
  let open Server_config in
  let open Context in
  validate_config config >>= function
  | `Ok ->
    begin
    async (fun () -> Imap_cmd.maintenance config);
    init_all config >>= fun (cert,sock,unix_sock) ->
    let rec connect f msgt sock cert =
      accept_conn msgt sock cert >>= fun (msgt,sock_c,(netr,netw)) ->
      let id = next_id () in
      f (
        fun () ->
        catch(
          fun () ->
            init_connection msgt netw >>= fun() ->
            let ctx =
              {id;client_id=ref [];commands=ref (Stack.create());
                netr=ref netr;netw=ref netw;state=ref
                Imaplet_types.State_Notauthenticated;mailbox=ref (Amailbox.empty());
                starttls=starttls config sock_c;highestmodseq=ref `None;
                noop_modseq = ref Int64.zero; capability=ref [];config;
                client_last_active = ref (Unix.gettimeofday());
                client_timed_out = Lwt_mutex.create ();
                compression = ref None} in
            Lwt_mutex.lock ctx.client_timed_out >>= fun () ->
            add_id ctx;
            Imap_cmd.client_requests msgt ctx >>= fun _ ->
            Log_.log `Info1 (Printf.sprintf "### closed client connection %s\n" (Int64.to_string id));
            rem_id id;
            try_close ctx.!netr >> try_close ctx.!netw >> try_close_sock sock_c 
        )
        (fun ex -> 
          Log_.log `Info1 (Printf.sprintf "### closed client connection %s: %s\n" (Int64.to_string id)
            (Printexc.to_string ex));
          rem_id id; try_close netr >> try_close netw >> try_close_sock sock_c) >>= fun () ->
        return `Ok
      ); 
      connect f msgt sock cert
    in
    let f a = a () in
    async (fun() -> connect f `Smtp unix_sock None);
    connect async `Client sock cert
    end
  | `Error e -> return (`Error e)
