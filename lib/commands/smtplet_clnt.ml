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

type result = [`Ok|`Error of string]

type post = ((from:string -> rcpt:string -> (unit -> string option Lwt.t) -> result Lwt.t) ->
  result Lwt.t)

(* from rcpt data *)
type t =
  {ip:string;port:int;ehlo:bool;
  log:([`None|`Error|`Debug|`Info1|`Info2|`Info3] -> string -> unit) option;
  post: post}

type post_info = {smtp:t;from:string;rcpt:string;feeder:(unit -> string option Lwt.t)}

let create ?log ip port ehlo post =
  {ip;port;ehlo;log;post}

(* it may take a bit of time for the smtp server to write a large message to the
 * imap server, so the response could take a long time. need to fix it on the
 * smtp server side, sent response right away, send a failure later if needed *)
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
let send_data t1 r w =
  write_server t1.smtp w "DATA" >>
  read_server_rc t1.smtp r "^250\\|354" >>= fun res ->
  if res = `Ok then (
    (* send one line of data at a time *)
    let rec send () =
      t1.feeder () >>= function
      | Some str -> write_server t1.smtp w str >> send ()
      | None -> write_server t1.smtp w "." >>= fun () ->
        read_server_rc t1.smtp r "^250"
    in
    catch (fun () ->
      send ()
    )
    (fun ex ->
      let msg = Printexc.to_string ex in
      dolog t1.smtp `Error (Printf.sprintf "### smtp client: failed to send data %s\n" msg);
      return (`Error msg)
    )
  ) else (
    return res
  )

(* send rcptto *)
let send_rcptto t1 r w =
  write_server t1.smtp w (Printf.sprintf "RCPT TO: <%s>" t1.rcpt) >>
  read_server_rc t1.smtp r "^250" >>= fun res ->
  if res = `Ok then
    send_data t1 r w
  else
    return res

(* send from *)
let send_from t r w =
  t.post (fun ~from ~rcpt feeder ->
    let t1 = {smtp = t; from; rcpt; feeder} in
    write_server t w (Printf.sprintf "MAIL FROM: <%s>" from) >>
    read_server_rc t r "^250" >>= fun res ->
    if res = `Ok then
      send_rcptto t1 r w
    else
      return res
  ) >>= fun res ->
  write_server t w "QUIT" >>
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
  | `Ok ->
    send_ehlo t r w (fun capabilities ->
      if is_capability capabilities "starttls" then
        send_starttls t sock r w
      else if t.ehlo then
        return (`Error "starttls is required")
      else
        send_from t r w
    )
  | res -> return res

(* start smtp state machine *)
let send_server t =
  catch (fun () ->
    client_send (`Inet (t.ip,t.port))
      (fun res sock ic oc -> greetings t sock ic oc) `Ok >>= fun res ->
    return res
  )
  (fun ex ->
    let err = Printexc.to_string ex in
    dolog t `Error (Printf.sprintf "### failed to send %s\n" err);
    return (`Error err)
  )
