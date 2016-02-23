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

let _dovecot = ref false
let _echo = ref false

module MapStr = Map.Make(String)

(* metadata - stores timers and echo to the terminal *)
module Meta =
  struct
    let timers = ref MapStr.empty
    let echo = ref true
  end

exception InvalidCommand

let compression = ref false

(* get user/password from the command line *)
let get_user str = 
  try
    let re = Re_posix.compile_pat ~opts:[`ICase] "^([^:]+):(.+)$" in
    let subs = Re.exec re str in
    let user = Re.get subs 1 in
    let pswd = Re.get subs 2 in
    Some (user,pswd)
  with Not_found -> raise InvalidCommand

(* process command line *)
let rec args i script addr port ssl user dovecot echo =
  if i >= Array.length Sys.argv then
    script,addr,port,ssl,user,dovecot,echo
  else
    match Sys.argv.(i) with 
    | "-script" -> args (i+2) Sys.argv.(i+1) addr port ssl user dovecot echo
    | "-address" -> args (i+2) script Sys.argv.(i+1) port ssl user dovecot echo
    | "-port" -> args (i+2) script addr (int_of_string (Sys.argv.(i+1))) ssl user dovecot echo
    | "-ssl" -> args (i+2) script addr port (bool_of_string (Sys.argv.(i+1))) user dovecot echo
    | "-user" -> args (i+2) script addr port ssl (get_user Sys.argv.(i+1)) dovecot echo
    | "-dovecot" -> args (i+1) script addr port ssl user true echo
    | "-echo" -> args (i+1) script addr port ssl user dovecot true
    | _ -> raise InvalidCommand

let usage () =
  Printf.fprintf stderr "usage: client -script [path] -address [address] \
  -port [port] [-ssl [true|false]] [-user [user:pswd]] [-dovecot] [-echo]\n%!"

(* script meta line contains start/stop timers and server response echo to the terminal *)
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

(* keep on reading the meta lines, if not then return the line (command) *)
let rec read_script strm =
  Lwt_stream.get strm >>= function
  | None -> return None
  | Some line ->
    match process_meta line with
    | `Ok -> read_script strm
    | `Done -> return (Some line)

(* preprocess the stream for the login and the meta header *)
let get_script file user =
  let strm = Lwt_io.read_lines file in
  let strm =
    (* if user/pswd passed in the command line then insert "login" command at the top of
     * the script stream 
     *)
    match user with
    | None -> strm
    | Some (user,pswd) ->
      let strm1 = Lwt_stream.of_list 
        ["a login " ^ user ^ " " ^ pswd;"^[^ ]+ (OK|BAD|NO)"] in
        Lwt_stream.append strm1 strm
  in
  (* process meta lines at the begining of the script *)
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

(* parse command line arguments and pass to the callback *)
let commands f =
  try 
    let script,addr,port,ssl,user,dovecot,echo = args 1 "" "127.0.0.1" 993 true None false false in
      try 
        f script addr port ssl user dovecot echo
      with ex -> Printf.printf "%s\n%!" (Printexc.to_string ex)
  with _ -> usage ()

(* initialize/connect socket *)
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

(* connect via ssl *)
let ssl_socket addr port =
  Nocrypto_entropy_lwt.initialize () >>
  X509_lwt.authenticator `No_authentication_I'M_STUPID >>= fun auth ->
  Tls_lwt.connect auth (addr,port)

(* connect regular or ssl *)
let connect addr port ssl =
  if ssl then
    ssl_socket addr port
  else
    socket addr port

(* in/out buffer if compression is enabled *)
let inbuffer = ref (Buffer.create 100)
let outbuffer = ref (Buffer.create 100)

let buff_size = 1024

(* uncompress *)
let inflate strm inbuff =
  let outbuff = Bytes.create buff_size in
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

(* reading/uncompression is done in chunks and
 * part of the chunk could be next compressed data stream
 * so the part is stored in the cache *)
let read_cache ic =
  if Buffer.length !inbuffer > 0 then (
    let contents = Buffer.contents !inbuffer in
    Buffer.clear !inbuffer;
    return (Some contents)
  ) else (
    let buff = Bytes.create buff_size in
    Lwt_io.read_into ic buff 0 buff_size >>= function
    | 0 -> return None
    | size -> return (Some (String.sub buff 0 size))
  )

(* return the line if not compressed,
 * uncompress otherwise *)
let zip_read ?count ic =
  if !compression = false then (
    match count with 
    | None -> Lwt_io.read_line_opt ic 
    | Some count -> 
      let buff = Bytes.create count in
      Lwt_io.read_into_exactly ic buff 0 count >>
      return (Some buff)
  ) else (
    (* initialize zlib *)
    let strm = Zlib.inflate_init true in
    (* recusevily uncompress chunks of data *)
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

(* read server response and output to the terminal if the echo is on *)
let read_net_echo ?count ic =
  zip_read ?count ic >>= function
  | None -> return None
  | Some line ->
    if !_echo || !Meta.echo then
      Printf.printf "%s\n%!" line;
    return (Some line)

(* write command to the server, echo to the terminal if echo is on *)
let write_echo oc command =
  let command = command ^ "\r\n" in
  if !_echo || !Meta.echo then
    Printf.printf "%s%!" command;
  let command = if !compression then Imap_crypto.do_compress command else command in
  Lwt_io.write oc command >> Lwt_io.flush oc

let send_append ic oc mailbox stream =
  let rec loop cnt =
    Lwt_stream.get stream >>= function
    | Some message ->
      Printf.printf "appending %d\r%!" cnt;
      let sz = if !_dovecot then 1 else 2 in
      let cmd = 
        Printf.sprintf "A%d APPEND %s {%d+}" cnt mailbox (String.length message + sz) in
      write_echo oc cmd >>
      write_echo oc message >>
      read_net_echo ic >>
      loop (cnt + 1)
    | None -> return ()
  in
  loop 1

(* send append to the server, append is expaned in that the actuall append data 
 * is referenced by a file *)
let handle_append ic oc mailbox msgfile =
  let (stream, push_stream) = Lwt_stream.create() in
  let t = Unix.gettimeofday() in
  let re = Re_posix.compile_pat ~opts:[`ICase;`Newline] "^message-id: ([^ \r\n]+)" in
  Utils.fold_email_with_file msgfile (fun _ message ->
    push_stream (Some message);
    return (`Ok())
  ) () >>= fun _ ->
  push_stream None;
  let t1 = Unix.gettimeofday() in
  let d = t1 -. t in
  (* adjust timers by the load time *)
  Meta.timers := MapStr.map (fun v -> v +. d) !Meta.timers;
  Printf.printf "done in-memory load %.02f\n%!" d;
  send_append ic oc mailbox stream >>= fun () ->
  Printf.printf "done send %.02f\n%!" (Unix.gettimeofday() -. t1);
  return ()

(* check if compression command *)
let is_compression command =
  if Regex.match_regex ~regx:"compress deflate" command then
    true
  else
    !compression

(* execute command at a time *)
let exec_command strm ic oc =
  read_script strm >>= function
  | None -> return `Done
  | Some command -> 
    let re_append = Re_posix.compile_pat "^append[ \t]+(([^ \t]+)|(\"[^\"]+\"))[ \t]+([^ \t]+)$" in
    let re_sleep = Re_posix.compile_pat "^sleep[ \t]+([0-9]+)$" in
    if Re.execp re_append command then (
      let subs = Re.exec re_append command in
      handle_append ic oc (Re.get subs 1) (Re.get subs 4) >>
      return `OkAppend
    ) else if Re.execp re_sleep command then (
      Printf.fprintf stderr "%s\n%!" command;
      let subs = Re.exec re_sleep command in
      Lwt_unix.sleep (float_of_string (Re.get subs 1)) >>
      return `OkSleep
    ) else (
      write_echo oc command >> return (`Ok (is_compression command))
    )

(* check if need to read a chunk,
 * i.e. fetch response for a body is like:
 * '* 1000 FETCH (FLAGS () BODY[] {7990}...)'
 * where {size} where size is the size of the data, ')' not counted
 *)
let re_chunk = Re_posix.compile_pat ~opts:[`ICase] "^* [^ ]+ FETCH.+ [{]([0-9]+)[}]$"

let is_read_chunk line =
  try
    let subs = Re.exec re_chunk line in
    let count = int_of_string (Re.get subs 1) + 1 in (* +1 for ')' *)
    Some count
  with Not_found -> None

(* recursively read server response, stop when the response matches regex if
 * specfied in the script *)
let rec read_response strm ic =
  read_script strm >>= function
  | None -> return `Done
  | Some regex ->
  let rec read ic =
    read_net_echo ic >>= function
    | None -> return `Ok
    | Some line ->
      match is_read_chunk line  with
      | Some count -> 
        begin
        read_net_echo ~count ic >>= function
        | None -> return `Ok
        | Some _ -> read ic
        end
      | None ->
        if Re.execp (Re_posix.compile_pat ~opts:[`ICase] regex) line then (
          if !Meta.echo = false then
            Printf.printf "%s\n%!" line;
          return `Ok
        ) else (
          read ic
        )
  in
  read ic

(* output the timers *)
let output_meta () =
  Printf.printf "---- timers\n%!";
  MapStr.iter (fun key value ->
    Printf.printf "%s %04f\n%!" key value
  ) !Meta.timers

let () =
  commands (fun script addr port ssl user dovecot echo ->
    _dovecot := dovecot;
    _echo := echo;
    Lwt_main.run (catch(fun() ->
        (* recursively execute stream commands *)
        let rec exec strm ic oc =
          exec_command strm ic oc >>= function
          | `Done -> return () (* done reading stream *)
          | `OkAppend | `OkSleep -> exec strm ic oc (* append is special, reads from a file *)
          | `Ok compr -> 
            read_response strm ic >>= function
            | `Done -> return ()
            | `Ok -> 
              compression := compr;
              exec strm ic oc 
        in
        Lwt_io.with_file ~mode:Lwt_io.Input script (fun file ->
          (* read the script file into the stream *)
          get_script file user >>= fun strm ->
          (* connect to the server *)
          connect addr port ssl >>= fun (ic,oc) ->
          (* read capability greeting from the server *)
          read_net_echo ic >>= fun _ -> 
          (* execute script commands *)
          exec strm ic oc >>= fun () ->
          (* print the meta, like execution timers *)
          output_meta ();
          return ()
        )
      )
      (fun ex -> Printf.fprintf stderr "client: fatal exception: %s %s"
        (Printexc.to_string ex) (Printexc.get_backtrace()); return()
      )
    )
  )
