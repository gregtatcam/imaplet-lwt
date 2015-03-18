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

(*
 structure of the script file
 server command, like: a1 select inbox
 clien response regex: [^ ]+ \\(OK\\|BAD\\|NO\\)
 all client responses are red untilf the regex is matched, 
 each server command should follow by the client response regex
 *)

exception InvalidCommand

let rec args i script addr port ssl =
  if i >= Array.length Sys.argv then
    script,addr,port,ssl
  else
    match Sys.argv.(i) with 
    | "-script" -> args (i+2) Sys.argv.(i+1) addr port ssl
    | "-address" -> args (i+2) script Sys.argv.(i+1) port ssl
    | "-port" -> args (i+2) script addr (int_of_string (Sys.argv.(i+1))) ssl
    | "-ssl" -> args (i+1) script addr port true
    | _ -> raise InvalidCommand

let usage () =
  Printf.fprintf stderr "usage: client -s [path] -a [address] -p [port]\n%!"

let commands f =
  try 
    let script,addr,port,ssl = args 1 "" "127.0.0.1" 143 false in
      try 
        f script addr port ssl
      with ex -> Printf.printf "%s\n%!" (Printexc.to_string ex)
  with _ -> usage ()

let socket addr port =
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_of_string "0.0.0.0",0) in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  Lwt_unix.bind socket sockaddr;
  let imapaddr = Unix.ADDR_INET (Unix.inet_addr_of_string addr, port) in
  Lwt_unix.connect socket imapaddr >>= fun() ->
  let ic = Lwt_io.of_fd ~mode:Lwt_io.input socket in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output socket in
  return (ic,oc)

let ssl_socket addr port =
  Tls_lwt.rng_init () >>
  X509_lwt.authenticator `No_authentication_I'M_STUPID >>= fun auth ->
  Tls_lwt.connect auth (addr,port)

let connect addr port ssl =
  if ssl then
    ssl_socket addr port
  else
    socket addr port

let read file =
  Lwt_io.read_line_opt file

let read_line_echo ic =
  Lwt_io.read_line_opt ic >>= function
  | None -> return None
  | Some line -> Printf.printf "%s\n%!" line; return (Some line)

let write_echo oc command =
  let command = command ^ "\r\n" in
  Printf.printf "%s%!" command;
  Lwt_io.write oc command >> Lwt_io.flush oc

let exec_command file oc =
  Lwt_io.read_line_opt file >>= function
  | None -> return `Done
  | Some command -> write_echo oc command >> return `Ok

let read_response file ic =
  Lwt_io.read_line_opt file >>= function
  | None -> return `Done
  | Some regex ->
  let rec read ic =
    read_line_echo ic >>= function
    | None -> return `Ok
    | Some line ->
        if Re.execp (Re_posix.compile_pat ~opts:[`ICase] regex) line then (
        return `Ok
      ) else (
        read ic
      )
  in
  read ic

let () =
  commands (fun script addr port ssl ->
    Lwt_main.run (catch(fun() ->
        connect addr port ssl >>= fun (ic,oc) ->
        read_line_echo ic >>= fun _ -> 
        Lwt_io.with_file ~mode:Lwt_io.Input script (fun file ->
          let rec exec () =
            exec_command file oc >>= function
            | `Done -> return ()
            | `Ok -> read_response file ic >>= function
              | `Done -> return ()
              | `Ok -> exec ()
          in
          exec ()
        )
      )
      (fun ex -> Printf.fprintf stderr "client: fatal exception: %s %s"
        (Printexc.to_string ex) (Printexc.get_backtrace()); return()
      )
    )
  )
