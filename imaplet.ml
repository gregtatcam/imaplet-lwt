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
open Core.Std
open Lwt

let update_config net port ssl tls =
  let open Server_config in
  srv_config.port := (match port with |None -> srv_config.!port|Some port->port);
  srv_config.ssl := (match ssl with |None -> srv_config.!ssl|Some ssl->ssl);
  srv_config.starttls := (srv_config.!ssl && (match tls with |None -> srv_config.!starttls|Some tls->tls));
  srv_config.addr := (match net with |None -> srv_config.!addr|Some net->net);
  Printf.printf "creating imap server on %s:%d:%b:%b\n%!"
    srv_config.!addr srv_config.!port srv_config.!ssl srv_config.!starttls

(**
 * handle command line
**)
let command =
  Command.basic
    ~summary:"run imaplet server"
      Command.Spec.(
      empty
      +> flag "-net" (optional string) ~doc:"network interface (optional, default all)"
      +> flag "-port" (optional int) ~doc:"port (optional, default 993)"
      +> flag "-ssl" (optional bool) ~doc:"ssl enabled (optional, default true)"
      +> flag "-tls" (optional bool) ~doc:"tls enabled (optional, default true, if ssl false then tls is ignored)"
      )
      (fun net port ssl tls () -> 
        Lwt_main.run (catch(fun() ->
            update_config net port ssl tls;
            Server.create ()
          )
          (fun ex -> Printf.printf "imaplet: fatal exception: %s %s"
            (Exn.to_string ex) (Exn.backtrace()); return()
          )
        )
      )

(**
 * start the server
**)
let () = 
  try
    Command.run command
  with Exit -> 
    Printf.printf "imaplet terminated: %s" (Exn.backtrace())
