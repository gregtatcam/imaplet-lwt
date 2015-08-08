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
open Imaplet
open Commands

exception InvalidCommand

let opt_val = function
  | None -> raise InvalidCommand
  | Some v -> v

let rec args i archive addr port ehlo from rcptto =
  if i >= Array.length Sys.argv then
    mbox,addr,port,ehlo,from,rcptto
  else
    match Sys.argv.(i) with 
    | "-archive" -> args (i+2) (Some Sys.argv.(i+1)) addr port ehlo from rcptto
    | "-address" -> args (i+2) archive (Some Sys.argv.(i+1)) port ehlo from rcptto
    | "-port" -> args (i+2) archive addr (int_of_string (Some Sys.argv.(i+1)) ehlo from rcptto
    | "-ehlo" -> args (i+1) archive addr port (bool_of_string (Sys.argv.(i+1))) from rcptto
    | "-from" -> args (i+2) archive addr port ehlo (Some Sys.argv.(i+1)) rcptto
    | "-rcptto" -> args (i+2) archive addr port ehlo from (Some Sys.argv.(i+1))
    | _ -> raise InvalidCommand

let usage () =
  Printf.fprintf stderr "usage: smtp_client -archive [path] -address [address] \
  -port [port] [-ehlo] [-from user@domain] [-rcptto user@domain]\n%!"

let commands f =
  try 
    let archive,addr,port,ehlo,from,rcptto = args None None None false None None in
    f (opt_val archive) (opt_val addr) (opt_val port) ehlo (opt_val from) (opt_val rcptto)
  with _ -> usage ()

let () =
  commands (fun archive addr port ehlo from rcptto
    Lwt_main.run (
      catch(fun() ->
        Utils.fold_email_with_file archive (fun acc message ->
          let t = Smtplet_clnt.create archive addr port ehlo from rcptto (fun () ->
          ) >>= function
          | `Ok
          | `Error err
        )
      )
      (fun ex -> Printf.fprintf stderr "client: fatal exception: %s %s"
        (Printexc.to_string ex) (Printexc.get_backtrace()); return()
      )
    )
  )
