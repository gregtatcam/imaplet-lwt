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
open Socket_utils

(* from rcpt data *)
type t =
  {ip:string;port:int;ehlo:bool;from:string;rcptto:string;
  log:([`None|`Error|`Debug|`Info1|`Info2|`Info3] -> string -> unit) option;
  line_of_data: (unit -> string option Lwt.t)}

let create ?log ip port ehlo from rcptto line_of_data =
  {ip;port;ehlo;from;rcptto;log;line_of_data}

let timeout = 120.

let dolog t level msg = 
  match t.log with
  | None -> ()
  | Some log -> log level msg

(*
 * write to the server
 *)
let write_server t w msg =
  dolog t `Info3 (Printf.sprintf "<-- server write: %s\n" msg);
  Lwt_io.write w (msg ^ "\r\n") 

(*
 * read response from the server
 *)
let read_server t r =
  Lwt.pick [
    (Lwt_unix.sleep timeout >> return None);
    Lwt_io.read_line_opt r;
  ] >>= function
  | Some str -> dolog t `Info3 (Printf.sprintf "--> server read: %s\n" str); return (Some str)
  | None -> return None

(*
 * read response from the server, 
 * match the response with the regex for Ok
 *)
let read_server_rc t r rc =
  read_server t r >>= function
  | Some str -> 
    if Regex.match_regex ~regx:rc str then
      return `Ok
    else
      return (`Error str)
  | None -> return (`Error "server terminated")

(*
 * send data to the server
 *)
let send_data t r w =
  write_server t w "DATA" >>
  read_server_rc t r "^250\\|354" >>= fun res ->
  if res = `Ok then (
    (* send one line of data at a time *)
    let rec send () =
      catch (fun () ->
        t.line_of_data () >>= function
        | Some str -> write_server t w str >> send ()
        | None -> write_server t w "." >>
          read_server_rc t r "^250" >>= fun res ->
          write_server t w "QUIT" >>
          return res
      )
      ( fun ex ->
        let msg = Printexc.to_string ex in
        dolog t `Error (Printf.sprintf "### failed to send data %s\n" msg); 
        return (`Error msg)
      )
    in
    send ()
  ) else (
    return res
  )

(* send rcptto *)
let send_rcptto t r w =
  write_server t w (Printf.sprintf "RCPT TO: <%s>" t.rcptto) >>
  read_server_rc t r "^250" >>= fun res ->
  if res = `Ok then
    send_data t r w 
  else
    return res

(* send from *)
let send_from t r w =
  write_server t w (Printf.sprintf "MAIL FROM: <%s>" t.from) >>
  read_server_rc t r "^250" >>= fun res ->
  if res = `Ok then
    send_rcptto t r w 
  else
    return res

(* read ehlo response *)
let rec read_ehlo t r = function
  | `Ok capabilities ->
    begin
    read_server t r >>= function
    | None -> return (`Error "server terminated")
    | Some str ->
      if Regex.match_regex ~regx:"^250\\([ -]\\)\\(.*\\)$" str then (
        let capabilities = (Str.matched_group 2 str) :: capabilities in
        (* last response must be "250 " *)
        if (Str.matched_group 1 str) = "-" then 
          read_ehlo t r (`Ok capabilities)
        else
          return (`Ok capabilities)
      ) else (
        return (`Error str)
      )
    end
  | res -> return res

(* send ehlo *)
let send_ehlo t r w (f:(string list -> [`Ok|`Error of string] Lwt.t)) =
  Lwt_unix.gethostname () >>= fun host ->
  write_server t w ((if t.ehlo then "EHLO " else "HELO ") ^ host) >>
  read_ehlo t r (`Ok []) >>= function
  | `Ok capabilities ->
    f capabilities
  | `Error err -> return (`Error err)

(* check for capability property *)
let is_capability capabilities capability =
  List.exists (fun cap -> 
    Regex.match_regex ~case:false ~regx:capability cap
  ) capabilities 

(* starttls with the server *)
let send_starttls t sock r w =
  write_server t w "STARTTLS" >>
  read_server_rc t r "^250\\|220" >>= fun res ->
  if res = `Ok then (
    dolog t `Info1 (Printf.sprintf "### starting tls to %s\n" t.ip);
    catch (fun () ->
    starttls_client t.ip sock () >>= fun (r,w) ->
    send_ehlo t r w (fun _ -> send_from t r w)
    ) (fun ex -> 
        dolog t `Error 
        (Printf.sprintf "### starttls exception: %s\n"
        (Printexc.to_string ex)); return (`Error "starttls failed")
      )
  ) else
      return res

(* start state machine *)
let greetings t sock r w =
  read_server_rc t r "^220" >>= function
  | `Ok -> send_ehlo t r w (fun capabilities ->
    if is_capability capabilities "starttls" then
      send_starttls t sock r w 
    else if t.ehlo then
      return (`Error "starttls is required")
    else
      send_from t r w)
  | res -> return res

(* try to send to available ports until success *)
let send_server t = 
  Lwt.pick [
    Lwt_unix.sleep timeout >> return `Timeout;
    (catch (fun () -> client_send (`Inet (t.ip,t.port)) (fun res sock ic oc ->
         greetings t sock ic oc) `Ok >>= fun res -> return (`Ok res)) 
    (fun ex -> dolog t `Error (Printf.sprintf "### failed to send %s\n"
      (Printexc.to_string ex)); return `Timeout (* try another port *))
    )
  ] >>= function
  | `Timeout -> dolog t `Info1 "### timeout\n"; return (`Error "timeout")
  | `Ok res -> dolog t `Info1 "### succeeded\n"; return res
