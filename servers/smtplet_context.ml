(*
 * Copyright (c) 2013-2014 Gregory Tsipenyuk <gt303@cam.ac.uk>
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
open Server_config

type netio = Lwt_io.input_channel * Lwt_io.output_channel*
  [`Reg of Lwt_unix.file_descr|`Ssl|`Starttls of Lwt_unix.file_descr]

(* relay_records = `None|DirectRelay of ip -> directly relay to ip|
 * MXRelay (priority * ip list) list, relay based on DNS MX:
 * from dns resolve/gethostbyname - list of domains, 
 * each domain has list of interfaces
 * if Some then the message must be relayed
 * user * doman * relay_records
 *)
type relayRecord = [`None|`DirectRelay of string*(int list)|`MXRelay of (int*string list) list]

type cmd_context = { 
  (* server configuration *) 
  config:imapConfig;
  (* user * password *) 
  auth:(string * string option) option;
  (* initial greeting type *) 
  grttype:[`Helo|`Ehlo|`Rset];
  (* connection io channel *)
  io:netio;
  (* user * domain *)
  from:(string*string option); 
  (* user * domain * relay *)
  rcpt:(string*string*relayRecord) list; 
  (* data content *)
  buff:Buffer.t} 
