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

exception InvalidCommand

let update_config net port ssl tls =
  let open Server_config in
  srv_config.port := (match port with |None -> srv_config.!port|Some port->port);
  srv_config.ssl := (match ssl with |None -> srv_config.!ssl|Some ssl->ssl);
  srv_config.starttls := (srv_config.!ssl && (match tls with |None -> srv_config.!starttls|Some tls->tls));
  srv_config.addr := (match net with |None -> srv_config.!addr|Some net->net);
  Printf.printf "imaplet: creating imap server on %s:%d:%b:%b\n%!"
    srv_config.!addr srv_config.!port srv_config.!ssl srv_config.!starttls

let rec args i net port ssl tls =
  let bval = function
  | "true" -> true
  | "false" -> false
  | _ -> raise InvalidCommand
  in
  if i >= Array.length Sys.argv then
    net,port,ssl,tls
  else
    match Sys.argv.(i) with 
    | "-net" -> args (i+2) (Some Sys.argv.(i+1)) port ssl tls
    | "-port" -> args (i+2) net (Some (int_of_string Sys.argv.(i+1))) ssl tls
    | "-ssl" -> args (i+2) net port (Some (bval Sys.argv.(i+1))) tls
    | "-tls" -> args (i+2) net port ssl (Some (bval Sys.argv.(i+1))) 
    | _ -> raise InvalidCommand

let usage () =
  Printf.printf "usage: imaplet -net [interface] -port [port] -ssl [true|false] -tls [true|false]\n%!"

let commands f =
  try 
    let net,port,ssl,tls = args 1 None None None None in
      try 
        f net port ssl tls
      with ex -> Printf.printf "%s\n%!" (Printexc.to_string ex)
  with _ -> usage ()

(**
 * start the server
**)
let () = 
  try
    commands 
      (fun net port ssl tls -> 
        Lwt_main.run (catch(fun() ->
            update_config net port ssl tls;
            Server.create ()
          )
          (fun ex -> Printf.printf "imaplet: fatal exception: %s %s"
            (Printexc.to_string ex) (Printexc.get_backtrace()); return()
          )
        )
      )
  with Exit -> 
    Printf.printf "imaplet: terminated: %s" (Printexc.get_backtrace())
