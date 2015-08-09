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
    append filename - needs to be expanded
 client response regex: [^ ]+ \\(OK\\|BAD\\|NO\\)
 all client responses are red untilf the regex is matched, 
 each server command should follow by the client response regex
 meta lines: 
   timer_start name
   timer_stop name
   echo [true,false]
 timer_start/stop measures the time lapse, outputs the measurement at the end
 echo outputs the data on the wire
 met can preceed the first command
 *)

module MapStr = Map.Make(String)

module Meta =
  struct
    let timers = ref MapStr.empty
    let echo = ref true
  end

exception InvalidCommand

let compression = ref false

let get_user str = 
  try
    let re = Re_posix.compile_pat ~opts:[`ICase] "^([^:]+):(.+)$" in
    let subs = Re.exec re str in
    let user = Re.get subs 1 in
    let pswd = Re.get subs 2 in
    Some (user,pswd)
  with Not_found -> raise InvalidCommand

let rec args i script addr port ssl user =
  if i >= Array.length Sys.argv then
    script,addr,port,ssl,user
  else
    match Sys.argv.(i) with 
    | "-script" -> args (i+2) Sys.argv.(i+1) addr port ssl user
    | "-address" -> args (i+2) script Sys.argv.(i+1) port ssl user
    | "-port" -> args (i+2) script addr (int_of_string (Sys.argv.(i+1))) ssl user
    | "-ssl" -> args (i+2) script addr port (bool_of_string (Sys.argv.(i+1))) user
    | "-user" -> args (i+2) script addr port ssl (get_user Sys.argv.(i+1))
    | _ -> raise InvalidCommand

let usage () =
  Printf.fprintf stderr "usage: client -script [path] -address [address] \
  -port [port] [-ssl [true|false]] [-user [user:pswd]]\n%!"

let process_meta line = 
  let re = Re_posix.compile_pat ~opts:[`ICase] "^(timer_start|timer_stop|echo)[ \t]+(.*)$" in
  try
    let subs = Re.exec re line in
    let meta = Re.get subs 1 in
    match meta with
    | "timer_start" ->
      Meta.timers := MapStr.add (Re.get subs 2) (Unix.gettimeofday ()) !Meta.timers;
      `Ok
    | "timer_stop" ->
      let t = MapStr.find (Re.get subs 2) !Meta.timers in
      Meta.timers := MapStr.add (Re.get subs 2) (Unix.gettimeofday () -. t) !Meta.timers;
      `Ok
    | "echo" ->
      Meta.echo := bool_of_string (Re.get subs 2);
      `Ok
    | _ -> `Done
  with Not_found -> `Done

let rec read_script strm =
  Lwt_stream.get strm >>= function
  | None -> return None
  | Some line ->
    match process_meta line with
    | `Ok -> read_script strm
    | `Done -> return (Some line)

let get_script file user =
  let strm = Lwt_io.read_lines file in
  let strm =
    match user with
    | None -> strm
    | Some (user,pswd) ->
      let strm1 = Lwt_stream.of_list 
        ["a login " ^ user ^ " " ^ pswd;"^[^ ]+ (OK|BAD|NO)"] in
        Lwt_stream.append strm1 strm
  in
  let rec header strm =
    Lwt_stream.peek strm >>= function
    | None -> return ()
    | Some line ->
      match process_meta line with
      | `Ok -> Lwt_stream.junk strm >> header strm
      | `Done -> return ()
  in
  header strm >>
  return strm

let commands f =
  try 
    let script,addr,port,ssl,user = args 1 "" "127.0.0.1" 993 true None in
      try 
        f script addr port ssl user
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
  Nocrypto_entropy_lwt.initialize () >>
  X509_lwt.authenticator `No_authentication_I'M_STUPID >>= fun auth ->
  Tls_lwt.connect auth (addr,port)

let connect addr port ssl =
  if ssl then
    ssl_socket addr port
  else
    socket addr port

let inbuffer = ref (Buffer.create 100)
let outbuffer = ref (Buffer.create 100)

let buff_size = 1024

let inflate strm inbuff =
  let outbuff = String.create buff_size in
  let rec _inflate inbuff offset len =
    let (fi, orig, size) = Zlib.inflate strm inbuff offset len 
      outbuff 0 buff_size Zlib.Z_SYNC_FLUSH in
    Buffer.add_string !outbuffer (String.sub outbuff 0 size);
    let offset = offset + orig in
    let len = len - orig in
    if fi then (
      Buffer.clear !inbuffer;
      if len <> 0 then (
        Buffer.add_string !inbuffer (String.sub inbuff offset len)
      );
      Zlib.inflate_end strm;
      true
    ) else if len = 0 then (
      false
    ) else (
      _inflate inbuff offset len
    )
  in
  _inflate inbuff 0 (String.length inbuff)

let read_cache ic =
  if Buffer.length !inbuffer > 0 then (
    let contents = Buffer.contents !inbuffer in
    Buffer.clear !inbuffer;
    return (Some contents)
  ) else (
    let buff = String.create buff_size in
    Lwt_io.read_into ic buff 0 buff_size >>= function
    | 0 -> return None
    | size -> return (Some (String.sub buff 0 size))
  )

let zip_read ic =
  if !compression = false then (
    Lwt_io.read_line_opt ic 
  ) else (
    let strm = Zlib.inflate_init true in
    let rec read () =
      read_cache ic >>= function
      | None -> return None
      | Some buff -> 
        if inflate strm buff then (
          let output = Buffer.contents !outbuffer in
          Buffer.clear !outbuffer;
          return (Some output) (*(Regex.replace ~regx:"[\r\n]*$" ~tmpl:""
          output))*)
        ) else (
          read ()
        )
    in
    read ()
  )

let read_net_echo ic =
  zip_read ic >>= function
  | None -> return None
  | Some line ->
    if !Meta.echo then
      Printf.printf "%s\n%!" line;
    return (Some line)

let write_echo oc command =
  let command = command ^ "\r\n" in
  if !Meta.echo then
    Printf.printf "%s%!" command;
  let command = if !compression then Imap_crypto.do_compress command else command in
  Lwt_io.write oc command >> Lwt_io.flush oc

(* send append to the server *)
let handle_append ic oc mailbox msgfile =
  Utils.fold_email_with_file msgfile (fun cnt message ->
    let cmd = 
      Printf.sprintf "A%d APPEND %s {%d+}" cnt mailbox (String.length message + 2) in
    write_echo oc cmd >>
    write_echo oc message >>
    read_net_echo ic >>
    return (`Ok (cnt + 1))
  ) 0 >>= fun _ ->
  return ()

let is_compression command =
  if Regex.match_regex ~regx:"compress deflate" command then
    true
  else
    !compression

let exec_command strm ic oc =
  read_script strm >>= function
  | None -> return `Done
  | Some command -> 
    if Regex.match_regex ~regx:"^append[ \t]+\\([^ \t]+\\)[ \t]+\\([^ \t]+\\)$" command then (
      handle_append ic oc (Str.matched_group 1 command) (Str.matched_group 2 command) >>
      return `OkAppend
    ) else (
      write_echo oc command >> return (`Ok (is_compression command))
    )

let read_response strm ic =
  read_script strm >>= function
  | None -> return `Done
  | Some regex ->
  let rec read ic =
    read_net_echo ic >>= function
    | None -> return `Ok
    | Some line ->
      if Re.execp (Re_posix.compile_pat ~opts:[`ICase] regex) line then (
        if !Meta.echo = false then
          Printf.printf "%s\n%!" line;
        return `Ok
      ) else (
        read ic
      )
  in
  read ic

let output_meta () =
  Printf.printf "---- timers\n%!";
  MapStr.iter (fun key value ->
    Printf.printf "%s %04f\n%!" key value
  ) !Meta.timers

let () =
  commands (fun script addr port ssl user ->
    Lwt_main.run (catch(fun() ->
        let rec exec strm ic oc =
          exec_command strm ic oc >>= function
          | `Done -> return ()
          | `OkAppend -> exec strm ic oc
          | `Ok compr -> 
            read_response strm ic >>= function
            | `Done -> return ()
            | `Ok -> 
              compression := compr;
              exec strm ic oc 
        in
        Lwt_io.with_file ~mode:Lwt_io.Input script (fun file ->
          get_script file user >>= fun strm ->
          connect addr port ssl >>= fun (ic,oc) ->
          read_net_echo ic >>= fun _ -> 
          exec strm ic oc >>= fun () ->
          output_meta ();
          return ()
        )
      )
      (fun ex -> Printf.fprintf stderr "client: fatal exception: %s %s"
        (Printexc.to_string ex) (Printexc.get_backtrace()); return()
      )
    )
  )
